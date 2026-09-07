(ns jj.bettong.capability.tavily
  "Searching through tavily."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn tavily
  "A `tavily_search` capability.

   Needs :api-key, or TAVILY_API_KEY.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (tavily {}))
  ([opts]
   (search/search (merge {:id :tavily
                          :tool-name "tavily_search"
                          :description "Search the web through Tavily, a search API built for language models."
                          :instructions "tavily_search searches the web. Cite the URLs you used."}
                         opts
                         {:backend (backends/tavily opts)}))))
