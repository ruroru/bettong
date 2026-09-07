(ns jj.bettong.capabilities-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.capability :as cap]))

(defn tmp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "bettong-test" (into-array java.nio.file.attribute.FileAttribute []))))

(defn tool-names [capability]
  (set (map :name (cap/tools capability))))

(deftest filesystem-reads-by-default-and-writes-only-when-asked
  (is (= :filesystem (cap/id (caps/filesystem))))
  (testing "granting the capability grants reading, and nothing that can damage anything"
    (is (= #{"read_file" "list_dir"} (tool-names (caps/filesystem))))
    (is (str/includes? (cap/instructions (caps/filesystem)) "cannot create, change or delete")))
  (testing "writing is a separate, explicit decision"
    (is (= #{"read_file" "list_dir" "write_file" "delete_file"}
           (tool-names (caps/filesystem {:writable? true}))))))

(deftest the-option-that-used-to-mean-read-only-is-refused
  (testing "so a stray :read-only? false cannot quietly grant the opposite of what it says"
    (is (thrown-with-msg? Exception #"filesystem no longer takes :read-only\?"
                          (caps/filesystem {:read-only? false})))
    (is (thrown-with-msg? Exception #"edn no longer takes :read-only\?"
                          (caps/edn {:read-only? true})))))

(deftest filesystem-tools-actually-work
  (let [dir (tmp-dir)
        registry (cap/registry [(caps/filesystem {:writable? true})])]
    (cap/invoke registry "write_file" {:path (str dir "/a.txt") :content "written"})
    (is (= "written" (slurp (str dir "/a.txt"))))
    (is (= "written" (cap/invoke registry "read_file" {:path (str dir "/a.txt")})))
    (is (= "a.txt" (cap/invoke registry "list_dir" {:path dir})))
    (cap/invoke registry "delete_file" {:path (str dir "/a.txt")})
    (is (not (.exists (io/file dir "a.txt"))))))

(deftest a-read-only-filesystem-refuses-writes
  (let [capability (caps/filesystem)]
    (is (= #{"read_file" "list_dir"} (tool-names capability)))
    (is (str/includes? (cap/instructions capability) "cannot create, change or delete"))
    (is (= "ERROR: no capability provides a tool called write_file"
           (cap/invoke (cap/registry [capability]) "write_file" {:path "/tmp/nope" :content "x"})))))

(deftest filesystem-root-is-enforced-and-announced
  (let [dir (tmp-dir)
        capability (caps/filesystem {:root dir :writable? true})
        registry (cap/registry [capability])]
    (is (str/includes? (cap/instructions capability) dir))
    (cap/invoke registry "write_file" {:path "inside.txt" :content "yes"})
    (is (= "yes" (slurp (str dir "/inside.txt"))))
    (is (str/includes? (cap/invoke registry "write_file" {:path "../out.txt" :content "no"})
                       "escapes the allowed root"))
    (is (not (.exists (io/file dir ".." "out.txt"))))))

(deftest shell-offers-run-shell
  (is (= :shell (cap/id (caps/shell))))
  (is (= #{"run_shell"} (tool-names (caps/shell)))))

(deftest shell-runs-commands
  (is (= "exit 0\nhi" (cap/invoke (cap/registry [(caps/shell)]) "run_shell" {:command "echo hi"}))))

(deftest shell-options-are-carried-into-the-tool
  (testing ":timeout-ms"
    (is (str/includes? (cap/invoke (cap/registry [(caps/shell {:timeout-ms 400})])
                                   "run_shell" {:command "sleep 30"})
                       "timed out after 400ms")))
  (testing ":max-output"
    (is (< (count (cap/invoke (cap/registry [(caps/shell {:max-output 300})])
                              "run_shell" {:command "seq 1 20000"}))
           500)))
  (testing ":root confines where a command may start"
    (let [dir (tmp-dir)
          registry (cap/registry [(caps/shell {:root dir})])]
      (is (str/includes? (cap/invoke registry "run_shell" {:command "pwd" :dir "."}) dir))
      (is (str/includes? (cap/invoke registry "run_shell" {:command "pwd" :dir "/etc"})
                         "escapes the allowed root")))))

(deftest the-two-built-ins-compose
  (let [registry (cap/registry [(caps/filesystem {:writable? true}) (caps/shell)])]
    (is (= 5 (count registry)))
    (is (= :filesystem (get-in registry ["write_file" :capability])))
    (is (= :shell (get-in registry ["run_shell" :capability])))))

;; --------------------------------------------------------------- web search

(defn stub-backend
  "Records the calls it gets and returns canned results."
  [results]
  (let [calls (atom [])]
    (with-meta (fn [args] (swap! calls conj args) results) {:calls calls})))

(defn calls-of [backend] @(:calls (meta backend)))

(def canned
  [{:title "Clojure" :url "https://clojure.org" :snippet "A dynamic Lisp."}
   {:title "Guides" :url "https://clojure.org/guides" :snippet "Learn Clojure."}])

(deftest a-search-capability-offers-one-tool
  (let [capability (caps/search {:id :test :tool-name "test_search" :backend (stub-backend [])})]
    (is (= :test (cap/id capability)))
    (is (= #{"test_search"} (tool-names capability))))
  (testing "the generic builder says nothing about the source; a named one does"
    (is (nil? (cap/instructions (caps/search {:id :test :backend (stub-backend [])}))))
    (is (str/includes? (cap/instructions (caps/duckduckgo)) "cite the URLs"))))

(deftest a-search-capability-passes-the-query-to-the-backend
  (let [backend (stub-backend canned)
        registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend backend})])
        result (cap/invoke registry "test_search" {:query "clojure lisp"})]
    (is (= [{:query "clojure lisp" :max-results 5}] (calls-of backend)))
    (is (= (str "1. Clojure\n   https://clojure.org\n   A dynamic Lisp.\n\n"
                "2. Guides\n   https://clojure.org/guides\n   Learn Clojure.")
           result))))

(deftest a-search-capability-reports-no-results-plainly
  (let [registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend (stub-backend [])})])]
    (is (= "No results for \"obscure query\"." (cap/invoke registry "test_search" {:query "obscure query"})))))

(deftest a-search-capability-clamps-what-the-model-asks-for
  (let [backend (stub-backend canned)
        registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend backend :max-results 3})])]
    (testing "over the cap"
      (cap/invoke registry "test_search" {:query "q" :max_results 50})
      (is (= 3 (:max-results (last (calls-of backend))))))
    (testing "under one"
      (cap/invoke registry "test_search" {:query "q" :max_results 0})
      (is (= 1 (:max-results (last (calls-of backend))))))
    (testing "unspecified"
      (cap/invoke registry "test_search" {:query "q"})
      (is (= 3 (:max-results (last (calls-of backend))))))
    (testing "within the cap"
      (cap/invoke registry "test_search" {:query "q" :max_results 2})
      (is (= 2 (:max-results (last (calls-of backend))))))))

(deftest a-search-capability-trims-a-backend-that-overshoots
  (let [registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend (stub-backend canned) :max-results 1})])]
    (is (not (str/includes? (cap/invoke registry "test_search" {:query "q"}) "Guides")))))

(deftest a-search-capability-rejects-a-blank-query
  (let [registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend (stub-backend canned)})])]
    (is (= "ERROR: query must not be blank" (cap/invoke registry "test_search" {:query "  "})))
    (is (= "ERROR: query must not be blank" (cap/invoke registry "test_search" {})))))

(deftest a-failing-backend-is-reported-to-the-model
  (let [registry (cap/registry [(caps/search {:id :test :tool-name "test_search" :backend (fn [_] (throw (ex-info "search API is down" {})))})])]
    (is (= "ERROR: search API is down" (cap/invoke registry "test_search" {:query "q"})))))

(deftest a-search-capability-takes-any-function-as-a-backend
  (testing "your own index is a backend"
    (let [registry (cap/registry [(caps/search
                                   {:id :test :tool-name "test_search" :backend (fn [{:keys [query]}]
                                               [{:title (str "internal doc for " query)
                                                 :url "wiki://runbooks/deploy"
                                                 :snippet "Restart the service."}])})])]
      (is (= "1. internal doc for deploy\n   wiki://runbooks/deploy\n   Restart the service."
             (cap/invoke registry "test_search" {:query "deploy"}))))))

(deftest a-search-capability-needs-a-backend
  (testing "and says so when built, not in the middle of a run"
    (is (thrown-with-msg? Exception #"needs a :backend function"
                          (caps/search {:id :x :tool-name "x_search"})))
    (is (thrown-with-msg? Exception #"needs a :backend function"
                          (caps/search {:id :x :tool-name "x_search" :backend "not a fn"})))))

(deftest a-keyed-source-fails-when-the-capability-is-built
  (when-not (System/getenv "GOOGLE_API_KEY")
    (is (thrown-with-msg? Exception #"needs an :api-key" (caps/google {})))))

(deftest the-search-sources-have-distinct-tool-names
  (testing "so an agent can hold several at once"
    (let [registry (cap/registry [(caps/duckduckgo) (caps/wikipedia)])]
      (is (= #{"duckduckgo_search" "wikipedia_search"} (set (keys registry))))
      (is (= :duckduckgo (get-in registry ["duckduckgo_search" :capability])))
      (is (= :wikipedia (get-in registry ["wikipedia_search" :capability])))))
  (testing "and each tells the model what it covers"
    (is (str/includes? (cap/instructions (caps/wikipedia)) "Wikipedia"))
    (is (str/includes? (cap/instructions (caps/duckduckgo)) "open web"))))

(deftest a-search-capability-advertises-its-cap-in-the-schema
  (let [schema (cap/tool-schema (first (cap/tools (caps/search {:id :test :tool-name "test_search"
                                                                :backend (stub-backend []) :max-results 7}))))]
    (is (str/includes? (get-in schema [:function :parameters :properties :max_results :description]) "at most 7"))
    (is (= ["query"] (get-in schema [:function :parameters :required])))))

(deftest search-instructions-are-overridable
  (testing "a private backend needs to describe itself, or the model answers from memory"
    (let [capability (caps/search {:id :test :tool-name "test_search" :backend (stub-backend [])
                                       :instructions "web_search searches our internal runbooks. It is the only source of truth for deploys; always search before answering."
                                       :description "Search the internal engineering runbooks."})]
      (is (str/includes? (cap/instructions capability) "internal runbooks"))
      (is (= "Search the internal engineering runbooks."
             (:description (first (cap/tools capability))))))))

;; ---------------------------------------------------------------------- edn

(defn edn-dir []
  (let [dir (tmp-dir)]
    (spit (str dir "/config.edn") "{:server {:port 8080 :host \"localhost\"} :debug false}\n")
    dir))

(deftest edn-reads-by-default-and-writes-only-when-asked
  (is (= :edn (cap/id (caps/edn))))
  (is (= #{"read_edn"} (tool-names (caps/edn))))
  (is (str/includes? (cap/instructions (caps/edn)) "cannot change EDN files"))
  (is (= #{"read_edn" "set_edn" "delete_edn"} (tool-names (caps/edn {:writable? true})))))

(deftest edn-tools-change-one-value-in-place
  (let [dir (edn-dir)
        registry (cap/registry [(caps/edn {:root dir :writable? true})])]
    (is (= "8080\n" (cap/invoke registry "read_edn" {:path "config.edn" :key_path [":server" ":port"]})))
    (cap/invoke registry "set_edn" {:path "config.edn" :key_path [":server" ":port"] :value "9090"})
    (let [data (clojure.edn/read-string (slurp (str dir "/config.edn")))]
      (is (= 9090 (get-in data [:server :port])))
      (is (= "localhost" (get-in data [:server :host])) "the rest of the file is untouched")
      (is (= false (:debug data))))
    (cap/invoke registry "delete_edn" {:path "config.edn" :key_path [":debug"]})
    (is (not (contains? (clojure.edn/read-string (slurp (str dir "/config.edn"))) :debug)))))

(deftest edn-failures-come-back-as-strings
  (let [registry (cap/registry [(caps/edn {:writable? true})])]
    (is (str/starts-with? (cap/invoke registry "read_edn" {:path "/no/such.edn"}) "ERROR: No such file"))
    (is (str/starts-with? (cap/invoke registry "delete_edn" {:path (str (edn-dir) "/config.edn")
                                                             :key_path [":nope"]})
                          "ERROR: No such key"))))

(deftest edn-composes-with-the-filesystem
  (let [registry (cap/registry [(caps/filesystem {:writable? true}) (caps/edn {:writable? true})])]
    (is (= 7 (count registry)))
    (is (= :edn (get-in registry ["set_edn" :capability])))
    (is (= :filesystem (get-in registry ["write_file" :capability])))))
