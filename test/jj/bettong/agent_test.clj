(ns jj.bettong.agent-test
  "The loop is tested against a scripted chat-fn, so no network is needed."
  (:require [jj.bettong.impl.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.agent :as agent]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.capability :as cap]
            [jj.bettong.test-server :as ts]
            [jj.bettong.usage :as usage]))

(defn tmp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "bettong-test" (into-array java.nio.file.attribute.FileAttribute []))))

(defn tool-call [id name args]
  {:id id :type "function"
   :function {:name name :arguments (json/write-str args)}})

(defn scripted
  "A chat-fn that returns the given assistant messages in order and records the
   requests it was handed."
  [messages]
  (let [remaining (atom messages)
        seen (atom [])]
    (with-meta
      (fn [request]
        (swap! seen conj request)
        (let [[m & more] @remaining]
          (when-not m (throw (ex-info "scripted chat-fn ran out of replies" {})))
          (reset! remaining more)
          m))
      {:seen seen})))

(defn requests [chat-fn] @(:seen (meta chat-fn)))

(defn tool-names [request]
  (set (map #(get-in % [:function :name]) (:tools request))))

(def fs [(caps/filesystem {:writable? true})])

;; ------------------------------------------------------------------ the loop

(deftest runs-a-tool-call-then-finishes
  (let [path (str (tmp-dir) "/foo.txt")
        chat (scripted [{:role "assistant" :content nil
                         :tool_calls [(tool-call "c1" "write_file" {:path path :content "foo"})]}
                        {:role "assistant" :content "Created the file."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "Create the file")]
    (is (= "foo" (slurp path)) "the tool actually ran")
    (is (= "Created the file." (:output result)))
    (is (true? (:done? result)))
    (is (= 2 (:steps result)))
    (is (= [{:tool "write_file"
             :capability :filesystem
             :args {:path path :content "foo"}
             :result (str "Wrote 3 bytes to " path)}]
           (:tool-calls result)))))

(deftest answers-without-tools
  (let [chat (scripted [{:role "assistant" :content "2"}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "What is 1+1?")]
    (is (= "2" (:output result)))
    (is (= [] (:tool-calls result)))
    (is (= 1 (:steps result)))))

(deftest feeds-tool-results-back-to-the-model
  (let [dir (tmp-dir)
        _ (spit (str dir "/a.txt") "hello")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "read_file" {:path (str dir "/a.txt")})]}
                        {:role "assistant" :content "It says hello."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "read it")
        second-request (second (requests chat))
        tool-msg (last (:messages second-request))]
    (is (= "tool" (:role tool-msg)))
    (is (= "c1" (:tool_call_id tool-msg)))
    (is (= "read_file" (:name tool-msg)))
    (is (= "hello" (:content tool-msg)))
    (testing "the assistant message is echoed back verbatim before the tool result"
      (is (= "assistant" (:role (nth (:messages second-request) 2)))))
    (is (= "It says hello." (:output result)))))

(deftest handles-several-tool-calls-in-one-turn
  (let [dir (tmp-dir)
        chat (scripted [{:role "assistant"
                         :tool_calls [(tool-call "c1" "write_file" {:path (str dir "/1.txt") :content "one"})
                                      (tool-call "c2" "write_file" {:path (str dir "/2.txt") :content "two"})]}
                        {:role "assistant" :content "Done."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "write both")]
    (is (= "one" (slurp (str dir "/1.txt"))))
    (is (= "two" (slurp (str dir "/2.txt"))))
    (is (= 2 (count (:tool-calls result))))
    (is (= ["c1" "c2"] (map :tool_call_id (filter #(= "tool" (:role %)) (:messages result)))))))

(deftest multi-step-conversation
  (let [dir (tmp-dir)
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path (str dir "/x") :content "1"})]}
                        {:role "assistant" :tool_calls [(tool-call "c2" "read_file" {:path (str dir "/x")})]}
                        {:role "assistant" :content "All good."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "do two things")]
    (is (= 3 (:steps result)))
    (is (= ["write_file" "read_file"] (map :tool (:tool-calls result))))
    (is (= "All good." (:output result)))))

(deftest stops-at-max-steps
  (let [dir (tmp-dir)
        looping (fn [_] {:role "assistant"
                         :tool_calls [(tool-call "c" "write_file" {:path (str dir "/loop") :content "x"})]})
        result (agent/run (agent/deepseek {:chat-fn looping :capabilities fs :max-steps 3}) "loop forever")]
    (is (false? (:done? result)))
    (is (nil? (:output result)))
    (is (= 3 (:steps result)))
    (is (= 3 (count (:tool-calls result))))))

(deftest tool-failure-is-reported-to-the-model-not-thrown
  (let [chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "read_file" {:path "/no/such/file"})]}
                        {:role "assistant" :content "That file does not exist."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "read it")]
    (is (= "That file does not exist." (:output result)))
    (is (re-find #"^ERROR: No such file" (:result (first (:tool-calls result)))))))

(deftest malformed-tool-arguments-are-reported
  (let [chat (scripted [{:role "assistant"
                         :tool_calls [{:id "c1" :type "function"
                                       :function {:name "write_file" :arguments "{not json"}}]}
                        {:role "assistant" :content "Sorry, retrying."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "go")]
    (is (re-find #"not valid JSON" (:result (first (:tool-calls result)))))
    (testing "the error tells the model how to fix it, so it can recover in one turn"
      (is (re-find #"separated by commas" (:result (first (:tool-calls result))))))
    (is (= "Sorry, retrying." (:output result)))))

(deftest edn-style-arrays-are-the-classic-malformation
  (testing "a model shown [:a :b] in a description copies it into JSON, which is invalid"
    (let [chat (scripted [{:role "assistant"
                           :tool_calls [{:id "c1" :type "function"
                                         :function {:name "set_edn"
                                                    :arguments "{\"path\": \"/tmp/c.edn\", \"key_path\": [\":a\" \":b\"]}"}}]}
                          {:role "assistant" :content "Retrying with commas."}])
          result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/edn {:writable? true})]}) "go")]
      (is (re-find #"separated by commas" (:result (first (:tool-calls result))))))))

(deftest tool-descriptions-show-json-not-edn-syntax
  (testing "every array example in a tool schema must itself be valid JSON"
    (doseq [capability [(caps/edn) (caps/filesystem) (caps/shell)
                        (caps/search {:id :s :tool-name "s_search" :backend (fn [_] [])})]
            tool (cap/tools capability)
            [_ param] (:parameters tool)
            :let [description (str (:description param))]
            example (re-seq #"\[[^\]]*\]" description)]
      (is (nil? (try (json/read-str example) nil
                     (catch Exception e e)))
          (str "in " (:name tool) ": " example " is not valid JSON")))))

;; ------------------------------------------------- capabilities are the gate

(deftest with-no-capabilities-the-agent-has-no-tools
  (let [chat (scripted [{:role "assistant" :content "I have no way to do that."}])
        result (agent/run (agent/deepseek {:chat-fn chat}) "Create /tmp/should-not-exist.txt")]
    (is (empty? (:tools (first (requests chat)))) "the model is not even offered any tool")
    (is (= "I have no way to do that." (:output result)))))

(deftest without-the-filesystem-capability-it-cannot-write
  (testing "even if the model tries the call anyway, nothing happens"
    (let [path (str (tmp-dir) "/never.txt")
          chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path path :content "x"})]}
                          {:role "assistant" :content "I do not have that tool."}])
          result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/shell)]}) "write it")]
      (is (not (.exists (io/file path))) "no file was created")
      (is (= "ERROR: no capability provides a tool called write_file"
             (:result (first (:tool-calls result)))))
      (is (nil? (:capability (first (:tool-calls result))))))))

(deftest without-the-shell-capability-it-cannot-run-commands
  (let [dir (tmp-dir)
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "run_shell" {:command (str "touch " dir "/shelled.txt")})]}
                        {:role "assistant" :content "No shell here."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "run it")]
    (is (not (.exists (io/file dir "shelled.txt"))))
    (is (str/starts-with? (:result (first (:tool-calls result))) "ERROR: no capability provides"))))

(deftest a-read-only-filesystem-really-is-read-only
  (let [dir (tmp-dir)
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path (str dir "/x.txt") :content "x"})]}
                        {:role "assistant" :content "Read-only."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/filesystem {})]}) "write")]
    (is (= #{"read_file" "list_dir"} (tool-names (first (requests chat)))))
    (is (not (.exists (io/file dir "x.txt"))))
    (is (str/starts-with? (:result (first (:tool-calls result))) "ERROR: no capability provides"))))

(deftest only-granted-capabilities-are-advertised
  (let [chat (scripted [{:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/shell)]}) "x")
    (is (= #{"run_shell"} (tool-names (first (requests chat)))))))

(deftest a-user-defined-capability-is-just-a-map
  (let [calls (atom [])
        weather {:id :weather
                 :instructions "Temperatures are in celsius."
                 :tools [{:name "get_weather"
                          :description "Current weather for a city."
                          :parameters {:city {:type "string" :required true}}
                          :handler (fn [{:keys [city]}] (swap! calls conj city) "17C and raining")}]}
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "get_weather" {:city "Vilnius"})]}
                        {:role "assistant" :content "It is 17C in Vilnius."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [weather]}) "weather in Vilnius?")]
    (is (= ["Vilnius"] @calls))
    (is (= {:tool "get_weather" :capability :weather :args {:city "Vilnius"} :result "17C and raining"}
           (first (:tool-calls result))))
    (is (= "It is 17C in Vilnius." (:output result)))
    (testing "its schema is sent to the model"
      (is (= [{:type "function"
               :function {:name "get_weather"
                          :description "Current weather for a city."
                          :parameters {:type "object" :properties {:city {:type "string"}} :required ["city"]}}}]
             (:tools (first (requests chat))))))))

(deftest a-stateful-capability-keeps-its-state-across-steps
  (let [todo (atom [])
        capability {:id :todo
                    :tools [{:name "add_todo"
                             :parameters {:item {:type "string" :required true}}
                             :handler (fn [{:keys [item]}] (swap! todo conj item) (str (count @todo) " items"))}]}
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "add_todo" {:item "milk"})]}
                        {:role "assistant" :tool_calls [(tool-call "c2" "add_todo" {:item "eggs"})]}
                        {:role "assistant" :content "Added both."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [capability]}) "add milk and eggs")]
    (is (= ["milk" "eggs"] @todo))
    (is (= ["1 items" "2 items"] (map :result (:tool-calls result))))))

(deftest capabilities-compose
  (let [chat (scripted [{:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/filesystem {})
                                                 (caps/shell)
                                                 {:id :extra :tools [{:name "extra" :handler (fn [_] "x")}]}]}) "x")
    (is (= #{"read_file" "list_dir" "run_shell" "extra"} (tool-names (first (requests chat)))))))

(deftest conflicting-capabilities-fail-loudly
  (let [a {:id :a :tools [{:name "clash" :handler (fn [_] "a")}]}
        b {:id :b :tools [{:name "clash" :handler (fn [_] "b")}]}]
    (is (thrown-with-msg? Exception #"Two capabilities both provide the tool clash"
                          (agent/run (agent/deepseek {:chat-fn (scripted []) :capabilities [a b]}) "x")))))

;; ---------------------------------------------------------- system prompting

(deftest capability-instructions-are-appended-to-the-system-prompt
  (let [chat (scripted [{:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities [{:id :c :instructions "Be careful with the laser."
                                                  :tools [{:name "fire" :handler (fn [_] "pew")}]}]}) "x")
    (let [prompt (:content (first (:messages (first (requests chat)))))]
      (is (str/starts-with? prompt agent/base-system-prompt))
      (is (str/ends-with? prompt "Be careful with the laser.")))))

(deftest system-override-keeps-capability-instructions
  (let [chat (scripted [{:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat :system "be terse"
                    :capabilities [{:id :c :instructions "Laser is hot."
                                    :tools [{:name "fire" :handler (fn [_] "pew")}]}]}) "x")
    (is (= "be terse\n\nLaser is hot." (:content (first (:messages (first (requests chat)))))))))

(deftest with-no-capabilities-the-prompt-is-just-the-base
  (let [chat (scripted [{:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat}) "x")
    (is (= agent/base-system-prompt (:content (first (:messages (first (requests chat)))))))))

;; ----------------------------------------------------------------- requests

(deftest sends-system-tools-and-user-message
  (let [chat (scripted [{:role "assistant" :content "hi"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities fs
                               :model "deepseek-v4-pro" :api-key "k" :base-url "http://x"}) "do the thing")
    (let [{:keys [messages tools model api-key base-url]} (first (requests chat))]
      (is (= "system" (:role (first messages))))
      (is (= {:role "user" :content "do the thing"} (second messages)))
      (is (= 2 (count messages)))
      (is (= 4 (count tools)))
      (is (= "deepseek-v4-pro" model))
      (is (= "k" api-key))
      (is (= "http://x" base-url)))))

;; ------------------------------------------------------------ approval hook

(deftest approve-fn-can-deny-a-tool-call
  (let [dir (tmp-dir)
        path (str dir "/blocked.txt")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path path :content "x"})]}
                        {:role "assistant" :content "I was not allowed to."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs :approve-fn (constantly false)}) "write it")]
    (is (not (.exists (io/file path))) "the handler never ran")
    (is (= "ERROR: write_file was not allowed" (:result (first (:tool-calls result)))))
    (is (= "I was not allowed to." (:output result)))))

(deftest approve-fn-can-give-a-reason
  (let [chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "run_shell" {:command "rm -rf /"})]}
                        {:role "assistant" :content "Understood."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/shell)]
                                      :approve-fn (constantly {:allowed? false :reason "too dangerous"})}) "clean up")]
    (is (= "ERROR: run_shell was not allowed: too dangerous"
           (:result (first (:tool-calls result)))))))

(deftest approve-fn-sees-the-tool-args-and-capability
  (let [seen (atom [])
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "run_shell" {:command "echo hi"})]}
                        {:role "assistant" :content "ok"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/shell)]
                     :approve-fn (fn [call] (swap! seen conj call) true)}) "go")
    (is (= [{:tool "run_shell" :args {:command "echo hi"} :capability :shell}] @seen))))

(deftest approve-fn-can-gate-a-whole-capability
  (let [dir (tmp-dir)
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "list_dir" {:path dir})
                                                        (tool-call "c2" "run_shell" {:command "echo no"})]}
                        {:role "assistant" :content "done"}])
        result (agent/run (agent/deepseek {:chat-fn chat
                                         :capabilities [(caps/filesystem) (caps/shell)]
                                         :approve-fn (fn [{:keys [capability]}]
                                                       (or (= :filesystem capability)
                                                           {:allowed? false :reason "shell is off today"}))}) "look around")]
    (is (= "(empty directory)" (:result (first (:tool-calls result)))))
    (is (= "ERROR: run_shell was not allowed: shell is off today"
           (:result (second (:tool-calls result)))))))

(deftest any-truthy-verdict-allows
  (testing "so an allowlist set, which returns the matched string, works as written"
    (let [chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "list_dir" {:path "/tmp"})]}
                          {:role "assistant" :content "ok"}])
          result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs
                                  :approve-fn (fn [{:keys [tool]}] (#{"list_dir"} tool))}) "go")]
      (is (not (re-find #"not allowed" (:result (first (:tool-calls result)))))))))

(deftest a-throwing-approve-fn-denies-rather-than-crashing-the-run
  (let [chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "list_dir" {:path "/tmp"})]}
                        {:role "assistant" :content "ok"}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs
                                :approve-fn (fn [_] (throw (ex-info "hook is broken" {})))}) "go")]
    (is (re-find #"approval hook threw: hook is broken" (:result (first (:tool-calls result)))))
    (is (true? (:done? result)))))

(deftest ask-in-terminal-reads-yes-or-no
  (is (true? (with-in-str "y\n" (agent/ask-in-terminal {:tool "run_shell" :args {:command "ls"}}))))
  (is (true? (with-in-str "YES\n" (agent/ask-in-terminal {:tool "run_shell" :args {}}))))
  (is (= {:allowed? false :reason "denied by the user"}
         (with-in-str "n\n" (agent/ask-in-terminal {:tool "run_shell" :args {}}))))
  (is (= {:allowed? false :reason "denied by the user"}
         (with-in-str "" (agent/ask-in-terminal {:tool "run_shell" :args {}})))))

;; ------------------------------------------------------------------- events

(deftest emits-progress-events
  (let [events (atom [])
        path (str (tmp-dir) "/e.txt")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path path :content "e"})]}
                        {:role "assistant" :content "done"}])]
    (agent/run (agent/deepseek {:chat-fn chat :capabilities fs :on-event #(swap! events conj %)}) "go")
    (is (= [:assistant :tool-call :assistant] (map :event @events)))
    (is (= "write_file" (:tool (second @events))))
    (is (= :filesystem (:capability (second @events))))))

;; -------------------------------------------------------------- arg decoding

(deftest parse-arguments-handles-the-shapes-deepseek-sends
  (is (= {:path "/tmp/a"} (agent/parse-arguments "{\"path\": \"/tmp/a\"}")))
  (is (= {} (agent/parse-arguments "")))
  (is (= {} (agent/parse-arguments nil)))
  (is (= {:already :parsed} (agent/parse-arguments {:already :parsed})))
  (is (thrown-with-msg? Exception #"not valid JSON" (agent/parse-arguments "{oops"))))

;; --------------------------------------------------------------- web search

(deftest searches-then-answers-from-the-results
  (let [queries (atom [])
        backend (fn [{:keys [query]}]
                  (swap! queries conj query)
                  [{:title "Clojure 1.12" :url "https://clojure.org/news" :snippet "Released September 2024."}])
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "web_search" {:query "latest clojure release"})]}
                        {:role "assistant" :content "Clojure 1.12, released September 2024 (https://clojure.org/news)."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/search {:id :web-search :tool-name "web_search" :backend backend})]}) "What is the latest Clojure release?")]
    (is (= ["latest clojure release"] @queries))
    (is (= :web-search (:capability (first (:tool-calls result)))))
    (is (str/includes? (:result (first (:tool-calls result))) "https://clojure.org/news"))
    (testing "the formatted results are what the model sees next"
      (is (str/includes? (:content (last (:messages (second (requests chat))))) "Released September 2024.")))
    (is (str/includes? (:output result) "1.12"))))

(deftest without-web-search-it-cannot-reach-the-internet
  (let [chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "web_search" {:query "anything"})]}
                        {:role "assistant" :content "I cannot search the web."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "look it up")]
    (is (= "ERROR: no capability provides a tool called web_search"
           (:result (first (:tool-calls result)))))))

(deftest search-composes-with-the-filesystem
  (let [dir (tmp-dir)
        backend (fn [_] [{:title "Result" :url "https://example.com" :snippet "Some fact."}])
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "web_search" {:query "some fact"})]}
                        {:role "assistant" :tool_calls [(tool-call "c2" "write_file" {:path (str dir "/notes.md")
                                                                                      :content "Some fact. https://example.com"})]}
                        {:role "assistant" :content "Searched and saved."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/search {:id :web-search :tool-name "web_search" :backend backend})
                                            (caps/filesystem {:writable? true})]}) "research it and save the notes")]
    (is (= [:web-search :filesystem] (map :capability (:tool-calls result))))
    (is (str/includes? (slurp (str dir "/notes.md")) "https://example.com"))))

;; ============================================================================
;; The whole loop over real HTTP: a stand-in DeepSeek on ring-http-exchange, a
;; stand-in search API, and the real capabilities in between. No :chat-fn stub.
;; ============================================================================

(defn- assistant-message [message]
  (ts/json-response (json/write-str {:choices [{:message message}]})))

(deftest the-whole-loop-runs-over-http
  (let [dir (tmp-dir)
        path (str dir "/from-the-loop.txt")
        turns (atom [{:role "assistant"
                      :tool_calls [(tool-call "c1" "write_file" {:path path :content "over http"})]}
                     {:role "assistant" :content "Wrote the file."}])]
    (ts/with-server [llm (fn [_] (assistant-message (ffirst (swap-vals! turns rest))))]
      (let [result (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "sk-test" :capabilities fs}) "write the file")]
        (is (= "over http" (slurp path)))
        (is (= "Wrote the file." (:output result)))
        (is (= 2 (:steps result)))
        (testing "the second request carries the transcript so far"
          (let [body (json/read-str (:body (second (ts/requests llm))))]
            (is (= ["system" "user" "assistant" "tool"] (map :role (:messages body))))
            (is (= (str "Wrote 9 bytes to " path) (:content (last (:messages body)))))
            (is (= "c1" (:tool_call_id (last (:messages body)))))))
        (testing "and every request is authenticated"
          (is (every? #(= "Bearer sk-test" (get-in % [:headers "authorization"])) (ts/requests llm))))))))

(deftest a-search-and-answer-run-over-http
  (let [turns (atom [{:role "assistant" :tool_calls [(tool-call "c1" "google_search" {:query "clojure"})]}
                     {:role "assistant" :content "Clojure is a Lisp (https://clojure.org)."}])]
    (ts/with-server [search (ts/json-response (json/write-str
                                               {:items [{:title "Clojure" :link "https://clojure.org"
                                                         :snippet "A dynamic Lisp."}]}))]
      (ts/with-server [llm (fn [_] (assistant-message (ffirst (swap-vals! turns rest))))]
        (let [result (agent/run (agent/deepseek {:base-url (:base-url llm)
                                 :api-key "k"
                                 :capabilities [(caps/google {:base-url (:base-url search)
                                                              :api-key "gk" :cx "cx"})]}) "what is clojure?")]
          (is (= "clojure" (get-in (ts/only-request search) [:params "q"])))
          (is (= "1. Clojure\n   https://clojure.org\n   A dynamic Lisp."
                 (:result (first (:tool-calls result)))))
          (is (str/includes? (:output result) "clojure.org")))))))

(deftest an-llm-failure-surfaces-rather-than-hanging
  (ts/with-server [llm {:status 500 :body "{\"error\":{\"message\":\"overloaded\"}}"}]
    (is (thrown? Exception (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :capabilities fs}) "go")))))

;; ---------------------------------------------------------- token accounting

(defn- assistant-with-usage [message usage]
  (ts/json-response (json/write-str {:choices [{:message message}] :usage usage})))

(deftest usage-adds-up-across-the-run
  (let [dir (tmp-dir)
        turns (atom [[{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path (str dir "/u.txt") :content "x"})]}
                      {:prompt_tokens 500 :completion_tokens 20 :total_tokens 520
                       :prompt_cache_hit_tokens 448 :prompt_cache_miss_tokens 52}]
                     [{:role "assistant" :content "Done."}
                      {:prompt_tokens 620 :completion_tokens 8 :total_tokens 628
                       :prompt_cache_hit_tokens 576 :prompt_cache_miss_tokens 44}]])]
    (ts/with-server [llm (fn [_] (let [[message usage] (ffirst (swap-vals! turns rest))]
                                   (assistant-with-usage message usage)))]
      (let [result (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :capabilities fs}) "write it")]
        (is (= {:requests 2
                :prompt-tokens 1120
                :completion-tokens 28
                :total-tokens 1148
                :cache-hit-tokens 1024
                :cache-miss-tokens 96
                :reasoning-tokens 0}
               (:usage result)))
        (testing "and it is summarizable and priceable"
          (is (= "2 requests 1148 tokens (1120 in, 28 out) [1024 cached, 91% of input]"
                 (usage/summary (:usage result))))
          (is (some? (usage/cost (:usage result) {:input 0.28 :output 0.42}))))))))

(deftest usage-is-reported-per-step-too
  (let [events (atom [])
        turns (atom [[{:role "assistant" :content "hi"} {:prompt_tokens 11 :completion_tokens 2 :total_tokens 13}]])]
    (ts/with-server [llm (fn [_] (let [[message usage] (ffirst (swap-vals! turns rest))]
                                   (assistant-with-usage message usage)))]
      (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :on-event #(swap! events conj %)}) "hi")
      (is (= 11 (get-in (first @events) [:usage :prompt-tokens]))))))

(deftest usage-survives-a-run-that-hits-the-step-limit
  (let [dir (tmp-dir)
        turns (atom (repeat [{:role "assistant" :tool_calls [(tool-call "c" "write_file" {:path (str dir "/l") :content "x"})]}
                             {:prompt_tokens 100 :completion_tokens 10 :total_tokens 110}]))]
    (ts/with-server [llm (fn [_] (let [[message usage] (ffirst (swap-vals! turns rest))]
                                   (assistant-with-usage message usage)))]
      (let [result (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :capabilities fs :max-steps 3}) "loop")]
        (is (false? (:done? result)))
        (is (= 3 (get-in result [:usage :requests])))
        (is (= 330 (get-in result [:usage :total-tokens])) "what the abandoned run still cost")))))

(deftest a-stubbed-chat-fn-still-counts-requests
  (let [chat (scripted [{:role "assistant" :content "hi"}])
        result (agent/run (agent/deepseek {:chat-fn chat}) "hi")]
    (is (= 1 (get-in result [:usage :requests])))
    (is (= 0 (get-in result [:usage :total-tokens])))))

;; ------------------------------------------------------------ edn capability

(deftest edits-an-edn-file-through-the-loop
  (let [dir (tmp-dir)
        file (str dir "/config.edn")
        _ (spit file "{:server {:port 8080 :host \"localhost\"} :debug false}\n")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "read_edn" {:path file :key_path [":server"]})]}
                        {:role "assistant" :tool_calls [(tool-call "c2" "set_edn" {:path file
                                                                                   :key_path [":server" ":port"]
                                                                                   :value "9090"})]}
                        {:role "assistant" :content "Port is now 9090."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/edn {:writable? true})]}) "change the port to 9090")
        data (clojure.edn/read-string (slurp file))]
    (is (= 9090 (get-in data [:server :port])))
    (is (= "localhost" (get-in data [:server :host])))
    (is (= [:edn :edn] (map :capability (:tool-calls result))))
    (is (= "Port is now 9090." (:output result)))))

(deftest without-the-edn-capability-it-cannot-edit-config
  (let [dir (tmp-dir)
        file (str dir "/config.edn")
        _ (spit file "{:a 1}")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "set_edn" {:path file :key_path [":a"] :value "2"})]}
                        {:role "assistant" :content "I cannot."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities fs}) "set a to 2")]
    (is (= "{:a 1}" (slurp file)))
    (is (str/starts-with? (:result (first (:tool-calls result))) "ERROR: no capability provides"))))

;; ------------------------------------------------ reading is what you get

(deftest granting-the-filesystem-does-not-grant-writing
  (testing "the tool is not offered at all"
    (let [chat (scripted [{:role "assistant" :content "ok"}])]
      (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/filesystem)]}) "x")
      (is (= #{"read_file" "list_dir"} (tool-names (first (requests chat)))))))
  (testing "and a model that tries anyway gets nowhere"
    (let [dir (tmp-dir)
          path (str dir "/unasked-for.txt")
          chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "write_file" {:path path :content "x"})]}
                          {:role "assistant" :content "I cannot write files."}])
          result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/filesystem)]}) "write it")]
      (is (not (.exists (io/file path))))
      (is (= "ERROR: no capability provides a tool called write_file"
             (:result (first (:tool-calls result))))))))

(deftest granting-edn-does-not-grant-changing-it
  (let [dir (tmp-dir)
        file (str dir "/config.edn")
        _ (spit file "{:a 1}")
        chat (scripted [{:role "assistant" :tool_calls [(tool-call "c1" "set_edn" {:path file :key_path [":a"] :value "2"})]}
                        {:role "assistant" :content "Read-only."}])
        result (agent/run (agent/deepseek {:chat-fn chat :capabilities [(caps/edn)]}) "change it")]
    (is (= "{:a 1}" (slurp file)))
    (is (str/starts-with? (:result (first (:tool-calls result))) "ERROR: no capability provides"))))
