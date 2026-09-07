(ns jj.bettong.capability.serper
  "Searching through serper."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn serper
  "A `serper_search` capability.

   Needs :api-key, or SERPER_API_KEY. Google results without a Google Cloud project.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (serper {}))
  ([opts]
   (search/search (merge {:id :serper
                          :tool-name "serper_search"
                          :description "Search the web for Google results through serper.dev."
                          :instructions "serper_search searches the web and returns Google's results. Cite the URLs you used."}
                         opts
                         {:backend (backends/serper opts)}))))
