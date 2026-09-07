(ns jj.bettong.capability.brave
  "Searching through brave."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn brave
  "A `brave_search` capability.

   Needs :api-key, or BRAVE_API_KEY.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (brave {}))
  ([opts]
   (search/search (merge {:id :brave
                          :tool-name "brave_search"
                          :description "Search the web through Brave Search."
                          :instructions "brave_search searches the web. Cite the URLs you used."}
                         opts
                         {:backend (backends/brave opts)}))))
