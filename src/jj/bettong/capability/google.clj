(ns jj.bettong.capability.google
  "Searching through google."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn google
  "A `google_search` capability.

   Needs :api-key (or GOOGLE_API_KEY) and :cx (or GOOGLE_CSE_ID), the id of a Programmable Search Engine set to search the entire web. Google caps a request at 10 results.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (google {}))
  ([opts]
   (search/search (merge {:id :google
                          :tool-name "google_search"
                          :description "Search the web through Google."
                          :instructions "google_search searches the web through Google. Cite the URLs you used."}
                         opts
                         {:backend (backends/google opts)}))))
