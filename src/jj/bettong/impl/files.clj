(ns jj.bettong.impl.files
  "Path resolution and the file operations behind the filesystem capability.
   Every path goes through `resolve-path`, which is where confinement to a root
   and the refusal to touch credential files live."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

;; ---------------------------------------------------------------- path safety

(def secret-paths
  "Files that hold credentials. Reading is the default grant, so these are
   refused unless a caller says otherwise - an agent that can read anything can
   read your keys, and everything it reads is uploaded."
  [#"/\.ssh/" #"/\.aws/credentials" #"/\.env(\.|$)" #"\.pem$" #"\.p12$"
   #"/\.netrc$" #"/\.npmrc$" #"/\.pypirc$" #"/\.git-credentials$"
   #"/id_(rsa|dsa|ecdsa|ed25519)" #"/\.docker/config\.json$" #"/\.kube/config$"
   #"/\.gnupg/" #"\.key$"])

(defn secret-path?
  [path]
  (boolean (some #(re-find % (str path)) secret-paths)))

(defn resolve-path
  "Resolves `path` to an absolute canonical File. When `root` is non-nil the
   result must stay inside it, otherwise an exception is thrown."
  ^File [{:keys [root allow-secrets?]} path]
  (when (str/blank? (str path))
    (throw (ex-info "path must not be blank" {:path path})))
  (let [given (io/file (str path))
        ;; relative paths hang off the root when there is one; absolute paths are
        ;; taken as-is and then have to survive the containment check below
        f (.getAbsoluteFile (if (and root (not (.isAbsolute given)))
                              (io/file root path)
                              given))
        ;; canonicalise the existing prefix so ".." and symlinks cannot escape
        canonical (.getCanonicalFile f)]
    (when root
      (let [root-str (str (.getCanonicalFile (io/file root)))
            here (str canonical)]
        (when-not (or (= here root-str)
                      (str/starts-with? here (str root-str File/separator)))
          (throw (ex-info "path escapes the allowed root"
                          {:path path :root root :resolved here})))))
    (when (and (not allow-secrets?) (secret-path? canonical))
      (throw (ex-info (str "refusing to touch a credential file: " canonical
                           " (pass :allow-secrets? true if you meant to)")
                      {:path (str canonical)})))
    canonical))

;; --------------------------------------------------------------- tool handlers

(defn write-file
  [opts {:keys [path content]}]
  (let [^File f (resolve-path opts path)
        content (or content "")]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f content)
    (format "Wrote %d bytes to %s" (count (.getBytes ^String content "UTF-8")) (str f))))

(defn read-file
  [opts {:keys [path]}]
  (let [^File f (resolve-path opts path)]
    (if (.isFile f)
      (slurp f)
      (throw (ex-info (str "No such file: " f) {:path (str f)})))))

(defn list-dir
  [opts {:keys [path]}]
  (let [^File f (resolve-path opts (or path "."))]
    (if (.isDirectory f)
      (let [entries (sort (map #(str (.getName ^File %) (when (.isDirectory ^File %) "/"))
                               (.listFiles f)))]
        (if (seq entries)
          (str/join "\n" entries)
          "(empty directory)"))
      (throw (ex-info (str "No such directory: " f) {:path (str f)})))))

(defn delete-file
  [opts {:keys [path]}]
  (let [^File f (resolve-path opts path)]
    (if (.isFile f)
      (do (io/delete-file f) (format "Deleted %s" (str f)))
      (throw (ex-info (str "No such file: " f) {:path (str f)})))))
