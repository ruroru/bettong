(ns jj.bettong.capability.wikipedia
  "Searching through wikipedia."
  (:require [jj.bettong.capability.search :as search]
            [jj.bettong.search :as backends]))

(defn wikipedia
  "A `wikipedia_search` capability.

   No API key. :language picks the edition, default en.

   Shared options: :max-results (5), :snippet-length, :timeout-ms, :base-url,
   :instructions, :description."
  ([] (wikipedia {}))
  ([opts]
   (search/search (merge {:id :wikipedia
                          :tool-name "wikipedia_search"
                          :description "Search Wikipedia for encyclopaedic facts: people, places, history, definitions."
                          :instructions "wikipedia_search searches Wikipedia. Prefer it for settled, encyclopaedic facts, and cite the article URLs."}
                         opts
                         {:backend (backends/wikipedia opts)}))))
