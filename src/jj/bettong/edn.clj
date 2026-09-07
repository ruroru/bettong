(ns jj.bettong.edn
  "Reading and updating values inside EDN files.

   Edits are spliced into the file's text, so comments, key order and hand
   formatting survive - see `jj.bettong.impl.edn-text`. Pass
   {:preserve-formatting? false} to rewrite the file from its data instead,
   which is pretty-printed and loses comments."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [jj.bettong.impl.edn-text :as edn-text]
            [jj.bettong.impl.files :as files]))

(defn parse-key
  "A path element arrives as text. It is read as EDN, so \":server\" is the
   keyword :server, \"0\" is the index 0, and \"\\\"name\\\"\" is the string
   \"name\". Anything unreadable is kept as a plain string key."
  [k]
  (if-not (string? k)
    k
    (try
      (let [parsed (edn/read-string k)]
        (if (nil? parsed) k parsed))
      (catch Exception _ k))))

(defn parse-path [path]
  (mapv parse-key (if (sequential? path) path [path])))

(defn parse-value
  "The new value, read as EDN so it can be any shape - a number, a string, a
   keyword, a map, a vector."
  [value]
  (if (string? value)
    (edn/read-string value)
    value))

(defn read-file
  "The file's data."
  [file]
  (edn/read-string (slurp (io/file file))))

(defn write-file!
  "Writes `data` back, pretty-printed so the file stays readable."
  [file data]
  (spit (io/file file) (with-out-str (pprint/pprint data)))
  data)

(defn get-in-file
  "The value at `path`, or nil. An empty path returns the whole file."
  [file path]
  (let [data (read-file file)
        path (parse-path path)]
    (if (seq path) (get-in data path) data)))

(defn set-in-file!
  "Sets `path` to `value`, creating intermediate maps as needed.
   Returns the file's new data.

   The edit is spliced into the file's text, so everything around it - comments
   included - is left exactly as it was. With {:preserve-formatting? false} the
   file is rewritten from its data and pretty-printed instead."
  ([file path value] (set-in-file! file path value {}))
  ([file path value {:keys [preserve-formatting?] :or {preserve-formatting? true}}]
   (let [path (parse-path path)
         value (parse-value value)]
     (when (empty? path)
       (throw (ex-info "path must name at least one key" {:file (str file)})))
     (if preserve-formatting?
       (let [updated (edn-text/set-in-str (slurp (io/file file)) path value)]
         (spit (io/file file) updated)
         (edn/read-string updated))
       (write-file! file (assoc-in (read-file file) path value))))))

(defn update-in-file!
  "Applies `f` to the value at `path` - the Clojure-side counterpart to
   set-in-file!, for when the new value depends on the old one.

     (update-in-file! \"config.edn\" [:server :port] inc)"
  [file path f & args]
  (let [path (parse-path path)]
    (when (empty? path)
      (throw (ex-info "path must name at least one key" {:file (str file)})))
    ;; work out the new value from the old one, then splice it in like any other
    (set-in-file! file path (apply f (get-in (read-file file) path) args))))

(defn delete-in-file!
  "Removes the key at `path`, leaving the formatting around it alone.
   Returns the file's new data."
  ([file path] (delete-in-file! file path {}))
  ([file path {:keys [preserve-formatting?] :or {preserve-formatting? true}}]
   (let [path (parse-path path)
         data (read-file file)]
     (when (empty? path)
       (throw (ex-info "path must name at least one key" {:file (str file)})))
     (when (= ::missing (get-in data path ::missing))
       (throw (ex-info (str "No such key: " (str/join " " path)) {:path path})))
     (if preserve-formatting?
       (let [updated (edn-text/delete-in-str (slurp (io/file file)) path)]
         (spit (io/file file) updated)
         (edn/read-string updated))
       (write-file! file
                    (if (= 1 (count path))
                      (dissoc data (first path))
                      (update-in data (butlast path) dissoc (last path))))))))

;; ------------------------------------------------------------ tool handlers

(defn- edn-file [opts path]
  (let [^java.io.File file (files/resolve-path opts path)]
    (when-not (.isFile file)
      (throw (ex-info (str "No such file: " file) {:path (str file)})))
    file))

(defn read-edn [opts {:keys [path key_path]}]
  (let [file (edn-file opts path)
        value (get-in-file file (or key_path []))]
    (if (nil? value)
      (format "No value at %s in %s" (str/join " " key_path) file)
      (with-out-str (pprint/pprint value)))))

(defn set-edn [opts {:keys [path key_path value]}]
  (let [file (edn-file opts path)]
    (set-in-file! file key_path value)
    (format "Set %s to %s in %s" (str/join " " key_path) value (str file))))

(defn delete-edn [opts {:keys [path key_path]}]
  (let [file (edn-file opts path)]
    (delete-in-file! file key_path)
    (format "Removed %s from %s" (str/join " " key_path) (str file))))
