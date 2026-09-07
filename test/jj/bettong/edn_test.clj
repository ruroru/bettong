(ns jj.bettong.edn-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.edn :as sut]
            [jj.bettong.impl.edn-text]))

(defn tmp-file
  ([] (tmp-file "{:server {:port 8080 :host \"localhost\"} :flags [:a :b] :debug false}\n"))
  ([content]
   (let [f (java.io.File/createTempFile "bettong-edn" ".edn")]
     (spit f content)
     (.deleteOnExit f)
     (str f))))

(defn data-of [file] (edn/read-string (slurp file)))

;; ------------------------------------------------------------ path decoding

(deftest path-elements-are-read-as-edn
  (is (= :server (sut/parse-key ":server")))
  (is (= 0 (sut/parse-key "0")))
  (is (= "name" (sut/parse-key "\"name\"")))
  (is (= 'sym (sut/parse-key "sym")))
  (testing "a non-string element is taken as it is"
    (is (= :already (sut/parse-key :already))))
  (testing "something unreadable stays a string key rather than blowing up"
    (is (= "]nonsense[" (sut/parse-key "]nonsense[")))
    (is (= "" (sut/parse-key "")))))

(deftest a-single-key-is-a-path
  (is (= [:server] (sut/parse-path ":server")))
  (is (= [:server :port] (sut/parse-path [":server" ":port"]))))

(deftest values-are-read-as-edn
  (is (= 8080 (sut/parse-value "8080")))
  (is (= "localhost" (sut/parse-value "\"localhost\"")))
  (is (= :prod (sut/parse-value ":prod")))
  (is (= [1 2 3] (sut/parse-value "[1 2 3]")))
  (is (= {:a 1} (sut/parse-value "{:a 1}")))
  (is (= false (sut/parse-value "false")))
  (testing "a value that is already data passes through"
    (is (= 42 (sut/parse-value 42)))))

;; -------------------------------------------------------------------- reads

(deftest reads-a-nested-value
  (let [f (tmp-file)]
    (is (= 8080 (sut/get-in-file f [":server" ":port"])))
    (is (= "localhost" (sut/get-in-file f [":server" ":host"])))
    (is (= [:a :b] (sut/get-in-file f [":flags"])))
    (is (= :b (sut/get-in-file f [":flags" "1"])))))

(deftest reads-the-whole-file-for-an-empty-path
  (let [f (tmp-file)]
    (is (= {:server {:port 8080 :host "localhost"} :flags [:a :b] :debug false}
           (sut/get-in-file f [])))))

(deftest a-missing-key-reads-as-nil
  (is (nil? (sut/get-in-file (tmp-file) [":nope"])))
  (is (nil? (sut/get-in-file (tmp-file) [":server" ":nope"]))))

;; ------------------------------------------------------------------- writes

(deftest sets-a-nested-value
  (let [f (tmp-file)]
    (sut/set-in-file! f [":server" ":port"] "9090")
    (is (= 9090 (get-in (data-of f) [:server :port])))
    (testing "and leaves everything else alone"
      (is (= "localhost" (get-in (data-of f) [:server :host])))
      (is (= [:a :b] (:flags (data-of f))))
      (is (= false (:debug (data-of f)))))))

(deftest sets-a-top-level-value
  (let [f (tmp-file)]
    (sut/set-in-file! f [":debug"] "true")
    (is (true? (:debug (data-of f))))))

(deftest creates-missing-intermediate-maps
  (let [f (tmp-file)]
    (sut/set-in-file! f [":db" ":pool" ":size"] "10")
    (is (= {:pool {:size 10}} (:db (data-of f))))))

(deftest sets-a-vector-element-by-index
  (let [f (tmp-file)]
    (sut/set-in-file! f [":flags" "1"] ":z")
    (is (= [:a :z] (:flags (data-of f))))))

(deftest sets-values-of-every-shape
  (let [f (tmp-file)]
    (sut/set-in-file! f [":a"] "{:nested [1 2 {:deep true}]}")
    (is (= {:nested [1 2 {:deep true}]} (:a (data-of f))))
    (sut/set-in-file! f [":b"] "nil")
    (is (contains? (data-of f) :b))
    (is (nil? (:b (data-of f))))))

(deftest an-empty-path-is-refused
  (testing "it would replace the whole file, which is never what was meant"
    (is (thrown-with-msg? Exception #"path must name at least one key"
                          (sut/set-in-file! (tmp-file) [] "1")))
    (is (thrown-with-msg? Exception #"path must name at least one key"
                          (sut/delete-in-file! (tmp-file) [])))))

(deftest the-file-stays-readable
  (let [f (tmp-file)]
    (sut/set-in-file! f [":server" ":port"] "9090")
    (testing "pretty-printed, and re-readable as edn"
      (is (str/includes? (slurp f) "\n"))
      (is (map? (data-of f))))))

;; ------------------------------------------------------------ update-in-file

(deftest updates-a-value-from-its-old-one
  (let [f (tmp-file)]
    (sut/update-in-file! f [":server" ":port"] inc)
    (is (= 8081 (get-in (data-of f) [:server :port])))))

(deftest update-takes-extra-arguments
  (let [f (tmp-file)]
    (sut/update-in-file! f [":flags"] conj :c)
    (is (= [:a :b :c] (:flags (data-of f))))))

;; ------------------------------------------------------------------ deletes

(deftest deletes-a-top-level-key
  (let [f (tmp-file)]
    (sut/delete-in-file! f [":debug"])
    (is (not (contains? (data-of f) :debug)))
    (is (contains? (data-of f) :server) "and only that key")))

(deftest deletes-a-nested-key
  (let [f (tmp-file)]
    (sut/delete-in-file! f [":server" ":host"])
    (is (= {:port 8080} (:server (data-of f))))))

(deftest deleting-a-missing-key-says-so
  (is (thrown-with-msg? Exception #"No such key: :nope"
                        (sut/delete-in-file! (tmp-file) [":nope"]))))

;; ------------------------------------------------------------ tool handlers

(deftest the-read-tool-pretty-prints-the-value
  (let [f (tmp-file)]
    (is (= "8080\n" (sut/read-edn {} {:path f :key_path [":server" ":port"]})))
    (is (str/includes? (sut/read-edn {} {:path f}) ":server"))))

(deftest the-read-tool-reports-a-missing-value
  (let [f (tmp-file)]
    (is (str/starts-with? (sut/read-edn {} {:path f :key_path [":nope"]}) "No value at :nope"))))

(deftest the-set-tool-confirms-what-it-did
  (let [f (tmp-file)]
    (is (= (format "Set :server :port to 9090 in %s" f)
           (sut/set-edn {} {:path f :key_path [":server" ":port"] :value "9090"})))
    (is (= 9090 (get-in (data-of f) [:server :port])))))

(deftest the-tools-refuse-a-file-that-is-not-there
  (doseq [handler [sut/read-edn sut/set-edn sut/delete-edn]]
    (is (thrown-with-msg? Exception #"No such file"
                          (handler {} {:path "/no/such/config.edn" :key_path [":a"] :value "1"})))))

(deftest the-tools-honour-a-root
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "bettong-edn" (into-array java.nio.file.attribute.FileAttribute [])))]
    (spit (str dir "/config.edn") "{:a 1}")
    (is (= "1\n" (sut/read-edn {:root dir} {:path "config.edn" :key_path [":a"]})))
    (is (thrown-with-msg? Exception #"escapes the allowed root"
                          (sut/read-edn {:root dir} {:path "../outside.edn" :key_path [":a"]})))))

;; ------------------------------------------- formatting, through the file api

(def commented
  (str ";; database settings -- see ops/runbook.md\n"
       "{:db {:url \"jdbc:postgresql://localhost/dev\"\n"
       "\n"
       "      ;; raised from 5 after the March incident\n"
       "      :pool-size 20}\n"
       "\n"
       "\n"
       " :debug false}\n"))

(deftest editing-a-file-keeps-its-comments-and-blank-lines
  (let [f (tmp-file commented)]
    (sut/set-in-file! f [":db" ":pool-size"] "30")
    (let [after (slurp f)]
      (is (= 30 (get-in (data-of f) [:db :pool-size])))
      (is (= (str/replace commented ":pool-size 20" ":pool-size 30") after)
          "only the value's own characters differ")
      (is (str/includes? after ";; raised from 5 after the March incident"))
      (is (str/includes? after ";; database settings -- see ops/runbook.md")))))

(deftest updating-a-file-from-its-old-value-keeps-formatting-too
  (let [f (tmp-file commented)]
    (sut/update-in-file! f [":db" ":pool-size"] inc)
    (is (= 21 (get-in (data-of f) [:db :pool-size])))
    (is (str/includes? (slurp f) ";; raised from 5 after the March incident"))))

(deftest deleting-from-a-file-keeps-the-comments-around-it
  (let [f (tmp-file commented)]
    (sut/delete-in-file! f [":debug"])
    (let [after (slurp f)]
      (is (= {:db {:url "jdbc:postgresql://localhost/dev" :pool-size 20}} (data-of f)))
      (is (str/includes? after ";; raised from 5 after the March incident"))
      (is (str/includes? after ";; database settings -- see ops/runbook.md")))))

(deftest a-full-rewrite-is-available-when-asked-for
  (let [f (tmp-file commented)]
    (sut/set-in-file! f [":db" ":pool-size"] "30" {:preserve-formatting? false})
    (is (= 30 (get-in (data-of f) [:db :pool-size])))
    (testing "which is what loses the comments"
      (is (not (str/includes? (slurp f) ";; raised from 5"))))))

(deftest an-edit-that-cannot-be-made-safely-leaves-the-file-alone
  (let [f (tmp-file commented)
        before (slurp f)]
    (with-redefs-fn {#'jj.bettong.impl.edn-text/node-of (fn [_] {:type :token :text "{{{"})}
      (fn []
        (is (thrown? Exception (sut/set-in-file! f [":debug"] "true")))))
    (is (= before (slurp f)) "not truncated, not half-written")))
