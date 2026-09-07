(ns jj.bettong.capability.duckduckgo
  "Searching through duckduckgo."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn duckduckgo
  "A `duckduckgo_search` capability.

   No API key. It scrapes DuckDuckGo's no-JavaScript endpoint, which sometimes answers a scraper with a challenge page instead of results.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (duckduckgo {}))
  ([opts]
   (search/search (merge {:id :duckduckgo
                          :tool-name "duckduckgo_search"
                          :description "Search the open web through DuckDuckGo."
                          :instructions "duckduckgo_search searches the open web. Use it for current information, and cite the URLs you used."}
                         opts
                         {:backend (backends/duckduckgo opts)}))))
