(ns jj.bettong.capability.search
  "Turning a search backend into a capability.

   Each search source ships as its own capability with its own tool name, so an
   agent can be given several at once and choose between them by name -
   `wikipedia_search` for an encyclopaedic fact, `duckduckgo_search` for the open
   web. One capability per source is also what lets the model see, in the tool
   descriptions, what each one actually covers."
  (:require [clojure.string :as str]
            [jj.bettong.search :as backends]))

(defn search
  "A search capability over any backend function. The named sources are built on
   this, and your own index goes in the same way:

     (search {:id :runbooks
              :tool-name \"runbook_search\"
              :backend (fn [{:keys [query max-results]}] (my-index/find query))
              :instructions \"runbook_search is the only source of truth for deploys.\"})

   Options:
     :backend      (fn [{:keys [query max-results]}] -> [{:title :url :snippet}])
     :id           capability id
     :tool-name    what the model calls it; must be unique across capabilities
     :description  the tool's own description
     :instructions system-prompt text. Say what this source covers when it is not
                   the open web, or the model answers from memory instead
     :max-results  results per search, and the cap on what the model may ask for
     :snippet-length  characters of each snippet kept (default 300)"
  [{:keys [backend id tool-name description instructions max-results snippet-length]}]
  (when-not (ifn? backend)
    (throw (ex-info "a search capability needs a :backend function" {:backend backend})))
  (let [limit (or max-results backends/default-max-results)
        tool-name (or tool-name "search")]
    {:id (or id :search)
     :instructions instructions
     :tools [{:name tool-name
              :description (or description
                               (str "Search and return the top results as title, URL and snippet. "
                                    "Snippets are short - follow up on a URL if you need the full page."))
              :parameters {:query {:type "string"
                                   :description "The search query. Use the words you would type into a search engine."
                                   :required true}
                           :max_results {:type "integer"
                                         :description (str "How many results to return, at most " limit ".")}}
              :handler (fn [{:keys [query max_results]}]
                         (when (str/blank? (str query))
                           (throw (ex-info "query must not be blank" {})))
                         (let [n (max 1 (min limit (or max_results limit)))
                               results (backend {:query query :max-results n})]
                           (backends/format-results query (take n results)
                                                    {:snippet-length (or snippet-length 300)})))}]}))
