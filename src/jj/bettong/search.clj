(ns jj.bettong.search
  "Search backends: the functions the search capabilities are built on.

   A backend is just (fn [{:keys [query max-results]}] -> [{:title :url :snippet}]),
   so anything can be one: a public search API, your own index, an internal wiki.
   The named ones below are conveniences."
  (:require [jj.bettong.impl.json :as json]
            [clojure.string :as str]
            [jj.potoroo.httpclient :as http])
  (:import [java.net URLDecoder URLEncoder]))

(def default-max-results 5)
(def default-timeout-ms 15000)

(def ^:private browser-user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36")

;; ------------------------------------------------------------------ requests

(defn- error-message
  "Search APIs put the useful part of a failure in the body, in their own shape."
  [body]
  (let [parsed (try (json/read-str body) (catch Exception _ nil))]
    (or (get-in parsed [:error :message])                 ; google
        (when (string? (:error parsed)) (:error parsed))  ; serper
        (:message parsed)                                 ; brave, tavily
        (:detail parsed)
        (when body (subs body 0 (min 200 (count body)))))))

(defn- endpoint
  "Backends take a :base-url so they can be pointed at a proxy, a regional
   endpoint, or a test server."
  [base-url default path]
  (str (or base-url default) path))

(defn- encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn query-string
  "Builds a url-encoded query string. The JDK client takes a url, not a params
   map, so we assemble it here."
  [params]
  (when (seq params)
    (str/join "&" (for [[k v] params]
                    (str (encode (if (keyword? k) (name k) k)) "=" (encode v))))))

(defn- fetch
  "Runs a request and returns the body, turning an API error into a message
   worth reading rather than a stack trace."
  [backend {:keys [url query-params] :as request}]
  (let [url (if-let [qs (query-string query-params)] (str url "?" qs) url)
        {:keys [status body]} (http/request (-> request
                                                (dissoc :query-params)
                                                (assoc :url url)))]
    (if (<= 200 status 299)
      body
      (throw (ex-info (format "%s search failed (HTTP %d): %s" (name backend) status (error-message body))
                      {:backend backend :status status})))))

;; ------------------------------------------------------------ html scrubbing

(def ^:private entities
  {"&amp;" "&" "&lt;" "<" "&gt;" ">" "&quot;" "\"" "&#39;" "'" "&#x27;" "'"
   "&nbsp;" " " "&apos;" "'" "&hellip;" "..." "&mdash;" "-" "&ndash;" "-"})

(defn unescape-html
  "Decodes the entities that turn up in search result titles and snippets."
  [s]
  (when s
    (-> (reduce (fn [acc [entity ch]] (str/replace acc entity ch)) s entities)
        (str/replace #"&#(\d+);" (fn [[_ code]] (str (char (parse-long code)))))
        (str/replace #"&#[xX]([0-9a-fA-F]+);"
                     (fn [[_ code]] (str (char (Long/parseLong code 16))))))))

(defn strip-tags
  "Search snippets come back with <b> highlighting; drop the markup."
  [s]
  (when s
    (-> s
        (str/replace #"(?s)<[^>]*>" "")
        unescape-html
        (str/replace #"\s+" " ")
        str/trim)))

;; --------------------------------------------------------------- duckduckgo

(defn- unwrap-redirect
  "DuckDuckGo wraps result links as //duckduckgo.com/l/?uddg=<encoded>&rut=..."
  [href]
  (if-let [encoded (second (re-find #"[?&]uddg=([^&]+)" (or href "")))]
    (URLDecoder/decode ^String encoded "UTF-8")
    (cond
      (str/starts-with? (or href "") "//") (str "https:" href)
      :else href)))

(defn parse-duckduckgo
  "Pulls results out of the html.duckduckgo.com results page."
  [html]
  (let [links (for [[_ attrs title] (re-seq #"(?s)<a\s+([^>]*class=\"[^\"]*result__a[^\"]*\"[^>]*)>(.*?)</a>" html)]
                {:title (strip-tags title)
                 :url (unwrap-redirect (second (re-find #"href=\"([^\"]+)\"" attrs)))})
        snippets (for [[_ snippet] (re-seq #"(?s)<a[^>]*class=\"[^\"]*result__snippet[^\"]*\"[^>]*>(.*?)</a>" html)]
                   (strip-tags snippet))]
    (->> (map (fn [link snippet] (assoc link :snippet snippet))
              links
              (concat snippets (repeat nil)))
         (remove #(str/blank? (:url %)))
         vec)))

(defn duckduckgo
  "Scrapes DuckDuckGo's no-JavaScript endpoint. No API key, but it is a public
   HTML page: DuckDuckGo may answer with a bot challenge instead of results,
   in which case you get none. For anything load-bearing, use a keyed backend."
  [{:keys [timeout-ms base-url]}]
  (fn [{:keys [query max-results]}]
    (->> (fetch :duckduckgo
                {:method :get
                 :url (endpoint base-url "https://html.duckduckgo.com" "/html/")
                 :query-params {"q" query}
                 :headers {"User-Agent" browser-user-agent
                           "Accept-Language" "en-US,en;q=0.9"}
                 :timeout-ms (or timeout-ms default-timeout-ms)})
         parse-duckduckgo
         (take (or max-results default-max-results))
         vec)))

;; ---------------------------------------------------------------- wikipedia

(defn parse-wikipedia
  "Article urls are built from the wiki that was searched, not a fixed one: a
   :language of \"lt\" searches lt.wikipedia, and its articles do not exist under
   en.wikipedia."
  ([body] (parse-wikipedia body "https://en.wikipedia.org"))
  ([body site]
   (->> (get-in (json/read-str body) [:query :search])
        (mapv (fn [{:keys [title snippet]}]
                {:title title
                 :url (str site "/wiki/" (str/replace title " " "_"))
                 :snippet (strip-tags snippet)})))))

(defn wikipedia
  "Searches Wikipedia through the MediaWiki API. No key needed.
   `:language` picks the edition, and the urls returned point at it."
  [{:keys [timeout-ms language base-url]}]
  (let [site (or base-url (format "https://%s.wikipedia.org" (or language "en")))]
    (fn [{:keys [query max-results]}]
      (parse-wikipedia
       (fetch :wikipedia
              {:method :get
               :url (str site "/w/api.php")
               :query-params {"action" "query" "list" "search" "format" "json"
                              "srsearch" query
                              "srlimit" (or max-results default-max-results)}
               :headers {"User-Agent" "bettong-clojure-agent"}
               :timeout-ms (or timeout-ms default-timeout-ms)})
       site))))

;; ------------------------------------------------------------ keyed backends

(defn- require-key [k env-var backend]
  (or k
      (System/getenv env-var)
      (throw (ex-info (format "The %s backend needs an :api-key, or %s in the environment"
                              backend env-var)
                      {:backend backend}))))

(defn parse-tavily [body]
  (->> (:results (json/read-str body))
       (mapv (fn [{:keys [title url content]}]
               {:title title :url url :snippet (strip-tags content)}))))

(defn tavily
  "https://tavily.com - a search API built for LLMs. Key: :api-key or TAVILY_API_KEY."
  [{:keys [api-key timeout-ms search-depth base-url]}]
  (let [key (require-key api-key "TAVILY_API_KEY" :tavily)]
    (fn [{:keys [query max-results]}]
      (parse-tavily
       (fetch :tavily
              {:method :post
               :url (endpoint base-url "https://api.tavily.com" "/search")
               :headers {"Authorization" (str "Bearer " key)
                         "Content-Type" "application/json"}
               :body (json/write-str {:query query
                                            :max_results (or max-results default-max-results)
                                            :search_depth (or search-depth "basic")})
               :timeout-ms (or timeout-ms default-timeout-ms)})))))

(defn parse-brave [body]
  (->> (get-in (json/read-str body) [:web :results])
       (mapv (fn [{:keys [title url description]}]
               {:title title :url url :snippet (strip-tags description)}))))

(defn brave
  "https://brave.com/search/api - key: :api-key or BRAVE_API_KEY."
  [{:keys [api-key timeout-ms base-url]}]
  (let [key (require-key api-key "BRAVE_API_KEY" :brave)]
    (fn [{:keys [query max-results]}]
      (parse-brave
       (fetch :brave
              {:method :get
               :url (endpoint base-url "https://api.search.brave.com" "/res/v1/web/search")
               :query-params {"q" query "count" (or max-results default-max-results)}
               :headers {"X-Subscription-Token" key "Accept" "application/json"}
               :timeout-ms (or timeout-ms default-timeout-ms)})))))

(defn parse-serper [body]
  (->> (:organic (json/read-str body))
       (mapv (fn [{:keys [title link snippet]}]
               {:title title :url link :snippet (strip-tags snippet)}))))

(defn serper
  "https://serper.dev - Google results. Key: :api-key or SERPER_API_KEY."
  [{:keys [api-key timeout-ms base-url]}]
  (let [key (require-key api-key "SERPER_API_KEY" :serper)]
    (fn [{:keys [query max-results]}]
      (parse-serper
       (fetch :serper
              {:method :post
               :url (endpoint base-url "https://google.serper.dev" "/search")
               :headers {"X-API-KEY" key
                         "Content-Type" "application/json"}
               :body (json/write-str {:q query :num (or max-results default-max-results)})
               :timeout-ms (or timeout-ms default-timeout-ms)})))))

(defn parse-google [body]
  (->> (:items (json/read-str body))
       (mapv (fn [{:keys [title link snippet]}]
               {:title title :url link :snippet (strip-tags snippet)}))))

(defn google
  "Google's official Custom Search JSON API.

   Needs two things, both from https://developers.google.com/custom-search/v1/overview:
     :api-key  an API key      (or GOOGLE_API_KEY)
     :cx       the id of a Programmable Search Engine (or GOOGLE_CSE_ID). Set that
               engine to 'Search the entire web' for general web search.

   The free tier is 100 queries a day. Google caps `num` at 10 per request."
  [{:keys [api-key cx timeout-ms base-url]}]
  (let [key (require-key api-key "GOOGLE_API_KEY" :google)
        cx (or cx
               (System/getenv "GOOGLE_CSE_ID")
               (throw (ex-info (str "The google backend needs a :cx search engine id, or GOOGLE_CSE_ID in "
                                    "the environment. Create one at https://programmablesearchengine.google.com")
                               {:backend :google})))]
    (fn [{:keys [query max-results]}]
      (parse-google
       (fetch :google
              {:method :get
               :url (endpoint base-url "https://www.googleapis.com" "/customsearch/v1")
               :query-params {"key" key
                              "cx" cx
                              "q" query
                              "num" (max 1 (min 10 (or max-results default-max-results)))}
               :timeout-ms (or timeout-ms default-timeout-ms)})))))

;; ---------------------------------------------------------------- formatting

(defn format-results
  "Renders results for the model: numbered, with the URL on its own line so it
   can be quoted back accurately."
  [query results {:keys [snippet-length] :or {snippet-length 300}}]
  (if (empty? results)
    (str "No results for " (pr-str query) ".")
    (->> results
         (map-indexed
          (fn [i {:keys [title url snippet]}]
            (let [snippet (when-not (str/blank? snippet)
                            (if (> (count snippet) snippet-length)
                              (str (subs snippet 0 snippet-length) "...")
                              snippet))]
              (str (inc i) ". " (or (not-empty title) "(untitled)") "\n"
                   "   " url
                   (when snippet (str "\n   " snippet))))))
         (str/join "\n\n"))))
