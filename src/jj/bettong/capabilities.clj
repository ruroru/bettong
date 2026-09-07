(ns jj.bettong.capabilities
  "Every capability that ships with bettong, in one place to require.

   Each also lives in its own namespace - `jj.bettong.capability.filesystem`,
   `jj.bettong.capability.wikipedia` and so on - if you would rather name them
   individually. None of them is on unless you pass it to an agent.

   The search sources are separate capabilities with separate tool names, so an
   agent can hold several at once and pick between them."
  (:require [jj.bettong.capability.brave :as brave]
            [jj.bettong.capability.duckduckgo :as duckduckgo]
            [jj.bettong.capability.edn :as edn]
            [jj.bettong.capability.filesystem :as filesystem]
            [jj.bettong.capability.google :as google]
            [jj.bettong.capability.search :as search]
            [jj.bettong.capability.serper :as serper]
            [jj.bettong.capability.shell :as shell]
            [jj.bettong.capability.tavily :as tavily]
            [jj.bettong.capability.wikipedia :as wikipedia]))

;; ------------------------------------------------------------------- the machine

(def filesystem
  "See `jj.bettong.capability.filesystem/filesystem`."
  filesystem/filesystem)

(def shell
  "See `jj.bettong.capability.shell/shell`."
  shell/shell)

(def edn
  "See `jj.bettong.capability.edn/edn`."
  edn/edn)

;; ------------------------------------------------------------------- searching

(def duckduckgo
  "See `jj.bettong.capability.duckduckgo/duckduckgo`. No API key."
  duckduckgo/duckduckgo)

(def wikipedia
  "See `jj.bettong.capability.wikipedia/wikipedia`. No API key."
  wikipedia/wikipedia)

(def google
  "See `jj.bettong.capability.google/google`. Needs a key and a search engine id."
  google/google)

(def tavily
  "See `jj.bettong.capability.tavily/tavily`. Needs a key."
  tavily/tavily)

(def brave
  "See `jj.bettong.capability.brave/brave`. Needs a key."
  brave/brave)

(def serper
  "See `jj.bettong.capability.serper/serper`. Needs a key."
  serper/serper)

(def search
  "See `jj.bettong.capability.search/search` - a search capability over your own
   backend function."
  search/search)
