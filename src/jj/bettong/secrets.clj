(ns jj.bettong.secrets
  "Keeping credentials out of transcripts.

   Anything a tool returns is sent to the model provider, so a command like
   `echo $API_KEY` does not just print a secret - it uploads one. Two defences
   here, both on by default: the child process never sees credentials in its
   environment, and whatever a tool does return is scrubbed of their values on
   the way back."
  (:require [clojure.string :as str])
  (:import [java.lang ProcessBuilder]))

(def secret-name
  "Environment variables whose names suggest they hold a credential."
  #"(?i)(KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|AUTH|PRIVATE)")

(defn secret-name? [n]
  (boolean (re-find secret-name (str n))))

(defn secret-values
  "The values worth scrubbing. Short ones are skipped: a two-character value is
   more likely to appear innocently in output than to be a credential."
  ([] (secret-values (System/getenv)))
  ([env]
   (into #{}
         (comp (filter (fn [[k _]] (secret-name? k)))
               (map (fn [[_ v]] v))
               (filter #(and (string? %) (>= (count %) 8))))
         env)))

(defn redact
  "`s` with any known credential value replaced. Cheap: a handful of string
   scans over output that is already capped in size."
  ([s] (redact s (secret-values)))
  ([s values]
   (if (str/blank? (str s))
     s
     (reduce (fn [acc v] (str/replace acc v "[redacted]")) (str s) values))))

(defn strip-env!
  "Removes credential-looking variables from a process's environment, so a
   command cannot read what it was never meant to see."
  [^ProcessBuilder pb]
  (let [env (.environment pb)]
    (doseq [k (vec (.keySet env))]
      (when (secret-name? k)
        (.remove env k)))
    pb))
