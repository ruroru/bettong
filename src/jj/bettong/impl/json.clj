(ns jj.bettong.impl.json
  "The one place JSON lives, so the parser behind it is a single-file change.

   `clojure.data.json` is chosen for its dependency profile rather than its
   speed: it is pure Clojure and pulls nothing else in, which matters more in a
   library other applications embed than raw throughput does on payloads this
   size. The JDK's own JSON API (JEP 540) lands in jdk.incubator.json in JDK 28
   and would slot in here."
  (:require [clojure.data.json :as json]))

(defn read-str
  "Parses JSON into Clojure data, with keyword keys."
  [s]
  (json/read-str s :key-fn keyword))

(defn write-str
  "Renders Clojure data as JSON. Slashes and non-ASCII are left as they are:
   both are valid JSON, and escaping them only makes the payload bigger and the
   transcript harder to read."
  [x]
  (json/write-str x :escape-slash false :escape-unicode false))
