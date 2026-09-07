(ns jj.bettong.search-test
  (:require [jj.bettong.impl.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.capability :as cap]
            [jj.bettong.search :as search]
            [jj.bettong.test-server :as ts]))

;; A trimmed copy of what html.duckduckgo.com returns: redirect-wrapped links,
;; entity-encoded ampersands, <b> highlighting in snippets, and a last result
;; whose snippet is missing.
(def duckduckgo-page
  "<div class=\"results\">
     <div class=\"result results_links web-result\">
       <h2 class=\"result__title\">
         <a rel=\"nofollow\" class=\"result__a\" href=\"//duckduckgo.com/l/?uddg=https%3A%2F%2Fclojure.org%2F&amp;rut=9f1\">Clojure</a>
       </h2>
       <a class=\"result__snippet\" href=\"//duckduckgo.com/l/?uddg=https%3A%2F%2Fclojure.org%2F\">Clojure is a <b>dynamic</b>, general-purpose programming language.</a>
     </div>
     <div class=\"result results_links web-result\">
       <h2 class=\"result__title\">
         <a rel=\"nofollow\" class=\"result__a\" href=\"//duckduckgo.com/l/?uddg=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FClojure&amp;rut=2b7\">Clojure &amp; the JVM - Wikipedia</a>
       </h2>
       <a class=\"result__snippet\" href=\"#\">Clojure runs on the <b>JVM</b>&nbsp;and the CLR.</a>
     </div>
     <div class=\"result results_links web-result\">
       <h2 class=\"result__title\">
         <a rel=\"nofollow\" class=\"result__a\" href=\"https://clojure.org/guides\">Guides</a>
       </h2>
     </div>
   </div>")

;; ---------------------------------------------------------------- scrubbing

(deftest unescape-html-decodes-entities
  (is (= "Clojure & the JVM" (search/unescape-html "Clojure &amp; the JVM")))
  (is (= "\"quoted\" 'single'" (search/unescape-html "&quot;quoted&quot; &#39;single&#39;")))
  (is (= "<tag>" (search/unescape-html "&lt;tag&gt;")))
  (testing "numeric and hex references"
    (is (= "A" (search/unescape-html "&#65;")))
    (is (= "A" (search/unescape-html "&#x41;"))))
  (is (nil? (search/unescape-html nil))))

(deftest strip-tags-removes-markup-and-collapses-space
  (is (= "a dynamic language" (search/strip-tags "a <b>dynamic</b> language")))
  (is (= "one two" (search/strip-tags "  one\n\n   two  ")))
  (is (= "Clojure & Lisp" (search/strip-tags "<b>Clojure</b>&nbsp;&amp; Lisp")))
  (is (nil? (search/strip-tags nil))))

;; -------------------------------------------------------------- duckduckgo

(deftest parses-duckduckgo-results
  (let [results (search/parse-duckduckgo duckduckgo-page)]
    (is (= 3 (count results)))
    (is (= {:title "Clojure"
            :url "https://clojure.org/"
            :snippet "Clojure is a dynamic, general-purpose programming language."}
           (first results)))
    (testing "entities in titles are decoded"
      (is (= "Clojure & the JVM - Wikipedia" (:title (second results)))))
    (testing "the redirect wrapper is unwound to the real url"
      (is (= "https://en.wikipedia.org/wiki/Clojure" (:url (second results)))))
    (testing "a plain href is left alone"
      (is (= "https://clojure.org/guides" (:url (nth results 2)))))
    (testing "a result with no snippet still comes through"
      (is (nil? (:snippet (nth results 2)))))))

(deftest a-bot-challenge-page-yields-no-results
  (testing "duckduckgo answers scrapers with a challenge page instead of results"
    (is (= [] (search/parse-duckduckgo "<html><head><title>DuckDuckGo</title></head><body>anomaly</body></html>")))
    (is (= [] (search/parse-duckduckgo "")))))

;; ------------------------------------------------------- the other backends

(deftest parses-wikipedia-results
  (let [body (json/write-str
              {:query {:search [{:title "Clojure" :snippet "A <span class=\"searchmatch\">Lisp</span> dialect"}
                                {:title "Rich Hickey" :snippet "created Clojure"}]}})]
    (is (= [{:title "Clojure"
             :url "https://en.wikipedia.org/wiki/Clojure"
             :snippet "A Lisp dialect"}
            {:title "Rich Hickey"
             :url "https://en.wikipedia.org/wiki/Rich_Hickey"
             :snippet "created Clojure"}]
           (search/parse-wikipedia body)))))

(deftest parses-google-results
  (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A dynamic Lisp."}
          {:title "Clojure - Wikipedia" :url "https://en.wikipedia.org/wiki/Clojure" :snippet "Created by Rich Hickey."}]
         (search/parse-google
          (json/write-str
           {:items [{:title "Clojure" :link "https://clojure.org" :snippet "A <b>dynamic</b> Lisp."}
                    {:title "Clojure - Wikipedia" :link "https://en.wikipedia.org/wiki/Clojure"
                     :snippet "Created by Rich Hickey."}]})))))

(deftest parses-tavily-results
  (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}]
         (search/parse-tavily (json/write-str
                               {:results [{:title "Clojure" :url "https://clojure.org" :content "A <b>Lisp</b>"}]})))))

(deftest parses-brave-results
  (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}]
         (search/parse-brave (json/write-str
                              {:web {:results [{:title "Clojure" :url "https://clojure.org" :description "A Lisp"}]}})))))

(deftest parses-serper-results
  (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}]
         (search/parse-serper (json/write-str
                               {:organic [{:title "Clojure" :link "https://clojure.org" :snippet "A Lisp"}]})))))

(deftest empty-payloads-parse-to-nothing
  (is (= [] (search/parse-wikipedia (json/write-str {:query {:search []}}))))
  (is (= [] (search/parse-tavily (json/write-str {:results []}))))
  (is (= [] (search/parse-brave (json/write-str {:web {:results []}}))))
  (is (= [] (search/parse-serper (json/write-str {}))))
  (testing "google omits :items entirely when nothing matched"
    (is (= [] (search/parse-google (json/write-str {:searchInformation {:totalResults "0"}}))))))

;; --------------------------------------------------------- backend resolving

(deftest google-needs-both-a-key-and-a-search-engine-id
  (testing "the key"
    (if (System/getenv "GOOGLE_API_KEY")
      (is (some? (search/google {:cx "x"})))
      (is (thrown-with-msg? Exception #"needs an :api-key, or GOOGLE_API_KEY"
                            (search/google {:cx "x"})))))
  (testing "and the search engine id, whose absence says where to make one"
    (if (System/getenv "GOOGLE_CSE_ID")
      (is (fn? (search/google {:api-key "k"})))
      (let [e (try (search/google {:api-key "k"}) (catch Exception e e))]
        (is (str/includes? (.getMessage e) ":cx search engine id"))
        (is (str/includes? (.getMessage e) "programmablesearchengine.google.com")))))
  (testing "given both, it builds"
    (is (fn? (search/google {:api-key "k" :cx "cse-id"})))))

(deftest keyed-backends-demand-a-key
  (doseq [[backend env] [[search/tavily "TAVILY_API_KEY"] [search/brave "BRAVE_API_KEY"]
                         [search/serper "SERPER_API_KEY"]]]
    (if (System/getenv env)
      (is (fn? (backend {})))
      (is (thrown-with-msg? Exception (re-pattern (str "needs an :api-key, or " env))
                            (backend {})))))
  (testing "an explicit key is enough"
    (is (fn? (search/tavily {:api-key "tvly-x"})))))

;; ---------------------------------------------------------------- formatting

(deftest formats-results-for-the-model
  (is (= (str "1. Clojure\n"
              "   https://clojure.org/\n"
              "   A dynamic Lisp.\n"
              "\n"
              "2. Guides\n"
              "   https://clojure.org/guides")
         (search/format-results "clojure"
                                [{:title "Clojure" :url "https://clojure.org/" :snippet "A dynamic Lisp."}
                                 {:title "Guides" :url "https://clojure.org/guides"}]
                                {}))))

(deftest formats-an-empty-result-set
  (is (= "No results for \"nothing at all\"." (search/format-results "nothing at all" [] {}))))

(deftest truncates-long-snippets
  (let [long-snippet (str/join (repeat 500 "x"))
        formatted (search/format-results "q" [{:title "T" :url "u" :snippet long-snippet}] {:snippet-length 50})]
    (is (str/includes? formatted (str (str/join (repeat 50 "x")) "...")))
    (is (< (count formatted) 120))))

(deftest handles-a-missing-title
  (is (str/includes? (search/format-results "q" [{:url "https://x.dev"}] {}) "1. (untitled)")))

;; ============================================================================
;; The same backends over a real HTTP server (ring-http-exchange), so the parts
;; a fixture cannot reach - urls, query params, auth headers, status handling,
;; timeouts - are covered without touching the network.
;; ============================================================================

(deftest duckduckgo-over-http
  (ts/with-server [s (fn [_] {:status 200 :headers {"Content-Type" "text/html"} :body duckduckgo-page})]
    (let [results ((search/duckduckgo {:base-url (:base-url s)}) {:query "clojure lisp" :max-results 2})
          request (ts/only-request s)]
      (is (= :get (:request-method request)))
      (is (= "/html/" (:uri request)))
      (is (= {"q" "clojure lisp"} (:params request)))
      (testing "it asks like a browser, which is what the endpoint expects"
        (is (str/includes? (get-in request [:headers "user-agent"]) "Mozilla/5.0"))
        (is (= "en-US,en;q=0.9" (get-in request [:headers "accept-language"]))))
      (testing "and :max-results is applied to the parsed page"
        (is (= 2 (count results)))
        (is (= "https://clojure.org/" (:url (first results))))))))

(deftest article-urls-point-at-the-edition-that-was-searched
  (testing "a :language of lt searches lt.wikipedia, whose articles do not exist on en"
    (let [body (json/write-str {:query {:search [{:title "Vytautas Didysis" :snippet "kunigaikstis"}]}})]
      (is (= "https://lt.wikipedia.org/wiki/Vytautas_Didysis"
             (:url (first (search/parse-wikipedia body "https://lt.wikipedia.org")))))
      (testing "and english is only the default"
        (is (= "https://en.wikipedia.org/wiki/Vytautas_Didysis"
               (:url (first (search/parse-wikipedia body)))))))))

(deftest the-language-option-reaches-both-the-request-and-the-urls
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:query {:search [{:title "Vilnius" :snippet "sostine"}]}}))]
    (let [results ((search/wikipedia {:base-url (:base-url s)}) {:query "Vilnius"})]
      (is (= (str (:base-url s) "/wiki/Vilnius") (:url (first results)))
          "an overridden base-url owns the article urls too"))))

(deftest wikipedia-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                       {:query {:search [{:title "Clojure" :snippet "A <b>Lisp</b>"}]}}))]
    (let [results ((search/wikipedia {:base-url (:base-url s)}) {:query "clojure" :max-results 3})
          request (ts/only-request s)]
      (is (= "/w/api.php" (:uri request)))
      (is (= {"action" "query" "list" "search" "format" "json" "srsearch" "clojure" "srlimit" "3"}
             (:params request)))
      (is (= [{:title "Clojure" :url (str (:base-url s) "/wiki/Clojure") :snippet "A Lisp"}] results)
          "the article url comes from the wiki that was searched"))))

(deftest google-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:items [{:title "Clojure" :link "https://clojure.org" :snippet "A Lisp"}]}))]
    (let [results ((search/google {:base-url (:base-url s) :api-key "AIza-k" :cx "cse-1"})
                   {:query "clojure" :max-results 4})
          request (ts/only-request s)]
      (is (= "/customsearch/v1" (:uri request)))
      (is (= {"key" "AIza-k" "cx" "cse-1" "q" "clojure" "num" "4"} (:params request)))
      (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}] results)))))

(deftest google-clamps-num-to-the-api-maximum
  (ts/with-server [s (ts/json-response (json/write-str {:items []}))]
    ((search/google {:base-url (:base-url s) :api-key "k" :cx "c"}) {:query "q" :max-results 50})
    (is (= "10" (get-in (ts/only-request s) [:params "num"])) "google rejects num > 10")))

(deftest tavily-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:results [{:title "Clojure" :url "https://clojure.org" :content "A Lisp"}]}))]
    (let [results ((search/tavily {:base-url (:base-url s) :api-key "tvly-k"}) {:query "clojure" :max-results 2})
          request (ts/only-request s)]
      (is (= :post (:request-method request)))
      (is (= "/search" (:uri request)))
      (is (= "Bearer tvly-k" (get-in request [:headers "authorization"])))
      (is (= {:query "clojure" :max_results 2 :search_depth "basic"}
             (json/read-str (:body request))))
      (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}] results)))))

(deftest brave-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:web {:results [{:title "Clojure" :url "https://clojure.org"
                                                          :description "A Lisp"}]}}))]
    (let [results ((search/brave {:base-url (:base-url s) :api-key "brave-k"}) {:query "clojure" :max-results 2})
          request (ts/only-request s)]
      (is (= "/res/v1/web/search" (:uri request)))
      (is (= "brave-k" (get-in request [:headers "x-subscription-token"])))
      (is (= {"q" "clojure" "count" "2"} (:params request)))
      (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}] results)))))

(deftest serper-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:organic [{:title "Clojure" :link "https://clojure.org" :snippet "A Lisp"}]}))]
    (let [results ((search/serper {:base-url (:base-url s) :api-key "serper-k"}) {:query "clojure" :max-results 2})
          request (ts/only-request s)]
      (is (= :post (:request-method request)))
      (is (= "serper-k" (get-in request [:headers "x-api-key"])))
      (is (= {:q "clojure" :num 2} (json/read-str (:body request))))
      (is (= [{:title "Clojure" :url "https://clojure.org" :snippet "A Lisp"}] results)))))

;; ---------------------------------------------------------- failure handling

(deftest an-api-error-carries-the-providers-own-message
  (testing "google"
    (ts/with-server [s {:status 400 :body (json/write-str
                                           {:error {:code 400 :message "API key not valid. Please pass a valid API key."}})}]
      (let [e (try ((search/google {:base-url (:base-url s) :api-key "bad" :cx "c"}) {:query "q"})
                   (catch Exception e e))]
        (is (= "google search failed (HTTP 400): API key not valid. Please pass a valid API key."
               (.getMessage e)))
        (is (= {:backend :google :status 400} (ex-data e))))))
  (testing "brave, whose errors are shaped differently"
    (ts/with-server [s {:status 422 :body (json/write-str {:message "Query is required"})}]
      (is (thrown-with-msg? Exception #"brave search failed \(HTTP 422\): Query is required"
                            ((search/brave {:base-url (:base-url s) :api-key "k"}) {:query "q"})))))
  (testing "serper, whose errors are shaped differently again"
    (ts/with-server [s {:status 403 :body (json/write-str {:error "Not enough credits"})}]
      (is (thrown-with-msg? Exception #"serper search failed \(HTTP 403\): Not enough credits"
                            ((search/serper {:base-url (:base-url s) :api-key "k"}) {:query "q"}))))))

(deftest a-non-json-error-body-still-reaches-the-caller
  (ts/with-server [s {:status 502 :headers {"Content-Type" "text/html"} :body "<html>502 Bad Gateway</html>"}]
    (is (thrown-with-msg? Exception #"wikipedia search failed \(HTTP 502\): <html>502 Bad Gateway</html>"
                          ((search/wikipedia {:base-url (:base-url s)}) {:query "q"})))))

(deftest a-slow-backend-hits-the-timeout
  (ts/with-server [s (fn [_] (Thread/sleep 3000) (ts/json-response "{}"))]
    (let [started (System/currentTimeMillis)]
      (is (thrown? Exception ((search/wikipedia {:base-url (:base-url s) :timeout-ms 300}) {:query "q"})))
      (is (< (- (System/currentTimeMillis) started) 2500) "it gave up rather than waiting out the request"))))

(deftest the-capability-runs-end-to-end-over-http
  (ts/with-server [s (ts/json-response (json/write-str
                                        {:items [{:title "Clojure" :link "https://clojure.org" :snippet "A Lisp"}
                                                 {:title "Guides" :link "https://clojure.org/guides" :snippet "Learn"}]}))]
    (let [registry (cap/registry [(caps/google {:base-url (:base-url s) :api-key "k" :cx "c"})])]
      (is (= "1. Clojure\n   https://clojure.org\n   A Lisp\n\n2. Guides\n   https://clojure.org/guides\n   Learn"
             (cap/invoke registry "google_search" {:query "clojure"}))))))
