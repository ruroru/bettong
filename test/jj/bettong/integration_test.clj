(ns jj.bettong.integration-test
  "Real calls to the DeepSeek API. Skipped by default; run with:
     DEEPSEEK_API_KEY=... lein test :integration"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.agent :as agent]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.search :as search]
            [jj.bettong.usage :as usage]))

(defn tmp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "bettong-it" (into-array java.nio.file.attribute.FileAttribute []))))

(def fs [(caps/filesystem {:writable? true})])
(def fs+shell [(caps/filesystem {:writable? true}) (caps/shell)])

(deftest ^:integration writes-a-file-end-to-end
  (let [path (str (tmp-dir) "/foo.txt")
        result (agent/run (agent/deepseek {:capabilities fs}) (format "Create file %s with content 'foo'" path))]
    (is (true? (:done? result)))
    (is (.isFile (io/file path)))
    (is (= "foo" (str/trim (slurp path))))
    (is (some #(= "write_file" (:tool %)) (:tool-calls result)))))

(deftest ^:integration reads-a-file-and-answers-about-it
  (let [dir (tmp-dir)]
    (spit (str dir "/secret.txt") "the password is bananas")
    (let [result (agent/run (agent/deepseek {:capabilities fs}) (format "Read %s/secret.txt and tell me the password." dir))]
      (is (true? (:done? result)))
      (is (str/includes? (str/lower-case (:output result)) "bananas")))))

(deftest ^:integration answers-a-plain-question
  (let [result (agent/run (agent/deepseek {:capabilities fs}) "Reply with just the word: pong")]
    (is (true? (:done? result)))
    (is (str/includes? (str/lower-case (:output result)) "pong"))
    (testing "whatever it pokes at, nothing is written"
      (is (not-any? #(= "write_file" (:tool %)) (:tool-calls result))))))

(deftest ^:integration multi-step-task
  (let [dir (tmp-dir)
        result (agent/run (agent/deepseek {:capabilities fs :max-steps 12}) (format "In directory %s create a.txt containing 'alpha', then create b.txt whose content is the content of a.txt uppercased." dir))]
    (is (true? (:done? result)))
    (is (= "alpha" (str/trim (slurp (str dir "/a.txt")))))
    (is (= "ALPHA" (str/trim (slurp (str dir "/b.txt")))))))

(deftest ^:integration uses-the-shell-for-a-real-task
  (let [dir (tmp-dir)]
    (doseq [n ["a" "b" "c"]] (spit (str dir "/" n ".txt") n))
    (let [result (agent/run (agent/deepseek {:capabilities fs+shell}) ;; the answer file must not be a .txt in the directory being counted, or
      ;; creating it changes the count and the question has two right answers
      (format "Using a shell command, count how many .txt files are in %s and write that number (digits only) to %s/count.out" dir dir))]
      (is (true? (:done? result)))
      (is (some #(= "run_shell" (:tool %)) (:tool-calls result)) "it reached for the shell")
      (is (= "3" (str/trim (slurp (str dir "/count.out"))))))))

(deftest ^:integration shell-failures-are-recoverable-by-the-model
  (let [dir (tmp-dir)
        result (agent/run (agent/deepseek {:capabilities fs+shell :max-steps 12}) (format "Run `cat %s/missing.txt` in the shell. If that fails, create the file with the content 'recovered' and read it again." dir))]
    (is (true? (:done? result)))
    (is (= "recovered" (str/trim (slurp (str dir "/missing.txt")))))))

;; --------------------------------------------- the gate, against a real model

(deftest ^:integration a-model-with-no-capabilities-cannot-touch-anything
  (let [path (str (tmp-dir) "/forbidden.txt")
        result (agent/run (agent/deepseek {}) (format "Create file %s with content 'foo'." path))]
    (is (true? (:done? result)))
    (is (empty? (:tool-calls result)))
    (is (not (.exists (io/file path))) "nothing was created")))

(deftest ^:integration a-model-without-the-shell-cannot-shell-out
  (let [dir (tmp-dir)
        result (agent/run (agent/deepseek {:capabilities [(caps/filesystem)]}) (format "Run the shell command `touch %s/shelled.txt`. If you cannot, say so." dir))]
    (is (true? (:done? result)))
    (is (not (.exists (io/file dir "shelled.txt"))))))

(deftest ^:integration a-read-only-model-cannot-write
  (let [dir (tmp-dir)]
    (spit (str dir "/data.txt") "original")
    (let [result (agent/run (agent/deepseek {:capabilities [(caps/filesystem)]}) (format "Replace the contents of %s/data.txt with the word 'changed'." dir))]
      (is (true? (:done? result)))
      (is (= "original" (slurp (str dir "/data.txt"))) "the file is untouched"))))

(deftest ^:integration root-holds-when-only-the-filesystem-is-granted
  (let [dir (tmp-dir)
        outside (str (tmp-dir) "/outside.txt")
        result (agent/run (agent/deepseek {:capabilities [(caps/filesystem {:root dir :writable? true})]}) (format "Create the file %s with content 'escaped'." outside))]
    (is (not (.exists (io/file outside))) "nothing was created outside the root")
    (testing "whatever it chose to do, no write escaped"
      ;; it may decline, may be refused by the tool, or may comply by writing
      ;; inside the root instead - all three are the sandbox working
      (is (every? (fn [{:keys [tool result]}]
                    (or (not= "write_file" tool)
                        (str/includes? (str result) dir)
                        (str/includes? (str result) "escapes the allowed root")))
                  (:tool-calls result))
          (str "a write went somewhere unexpected: " (:tool-calls result))))))

(deftest ^:integration approval-hook-blocks-a-real-model
  (let [dir (tmp-dir)
        asked (atom [])
        result (agent/run (agent/deepseek {:capabilities fs+shell
                           :approve-fn (fn [call]
                                         (swap! asked conj (:tool call))
                                         {:allowed? false :reason "destructive commands are disabled"})}) (format "Delete every file in %s using the shell." dir))]
    (is (seq @asked) "the hook was consulted")
    (is (every? #(str/includes? (str (:result %)) "was not allowed") (:tool-calls result)))))

;; ------------------------------------------- a user-defined capability, live

(deftest ^:integration a-custom-capability-is-used-by-a-real-model
  (let [asked (atom [])
        rates {"EUR" 1.0 "USD" 1.08 "GBP" 0.85}
        exchange {:id :exchange
                  :instructions "Use get_rate for currency conversions; never guess a rate."
                  :tools [{:name "get_rate"
                           :description "The exchange rate from EUR to the given currency."
                           :parameters {:currency {:type "string"
                                                   :description "ISO currency code, e.g. USD."
                                                   :required true}}
                           :handler (fn [{:keys [currency]}]
                                      (swap! asked conj currency)
                                      (if-let [rate (get rates (str/upper-case (str currency)))]
                                        (str "1 EUR = " rate " " (str/upper-case currency))
                                        (str "ERROR: unknown currency " currency)))}]}
        result (agent/run (agent/deepseek {:capabilities [exchange]}) "How many US dollars is 50 EUR? Use your tools for the rate.")]
    (is (true? (:done? result)))
    (is (= ["USD"] @asked) "the model called the capability we defined")
    (is (str/includes? (:output result) "54"))))

(deftest ^:integration a-capability-can-be-the-only-thing-it-has
  (testing "an agent granted just one custom tool cannot fall back to files"
    (let [path (str (tmp-dir) "/notes.txt")
          notes (atom [])
          notebook {:id :notebook
                    :instructions "Save anything the user asks you to remember with remember_note."
                    :tools [{:name "remember_note"
                             :description "Store a note."
                             :parameters {:text {:type "string" :required true}}
                             :handler (fn [{:keys [text]}] (swap! notes conj text) "saved")}]}
          result (agent/run (agent/deepseek {:capabilities [notebook]}) (format "Remember that the meeting is on Tuesday. Also write it to %s." path))]
      (is (true? (:done? result)))
      (is (= 1 (count @notes)))
      (is (str/includes? (str/lower-case (first @notes)) "tuesday"))
      (is (not (.exists (io/file path))) "it had no filesystem capability"))))

;; ------------------------------------------------ web search, live over HTTP

(deftest ^:integration wikipedia-backend-really-searches
  (let [results ((search/wikipedia {}) {:query "Rich Hickey Clojure" :max-results 3})]
    (is (= 3 (count results)))
    (is (every? #(str/starts-with? (:url %) "https://en.wikipedia.org/wiki/") results))
    (is (every? #(seq (:title %)) results))
    (is (some #(str/includes? (str/lower-case (str (:title %) (:snippet %))) "clojure") results))))

(deftest ^:integration the-model-searches-and-cites
  (let [seen (atom [])
        capability (caps/search {:id :wikipedia :tool-name "wikipedia_search"
                                   :backend (let [wiki (search/wikipedia {})]
                                                (fn [args] (swap! seen conj (:query args)) (wiki args)))
                                     :max-results 3})
        result (agent/run (agent/deepseek {:capabilities [capability]}) "Who created the Clojure programming language? Search for it and cite the URL you used.")]
    (is (true? (:done? result)))
    (is (seq @seen) "it actually searched")
    (is (str/includes? (:output result) "Hickey"))
    (is (str/includes? (:output result) "wikipedia.org"))))

(deftest ^:integration search-results-reach-the-filesystem
  (let [dir (tmp-dir)
        result (agent/run (agent/deepseek {:capabilities [(caps/wikipedia {:max-results 3})
                                          (caps/filesystem {:root dir :writable? true})]
                           :max-steps 12}) (format "Search for the Clojure programming language, then write the URL of the top result to %s/source.txt" dir))]
    (is (true? (:done? result)))
    (is (= [:wikipedia :filesystem] (distinct (map :capability (:tool-calls result)))))
    (is (str/includes? (slurp (str dir "/source.txt")) "wikipedia.org"))))

;; ------------------------------------------------- token accounting, for real

(deftest ^:integration reports-what-the-run-actually-cost
  (let [path (str (tmp-dir) "/counted.txt")
        result (agent/run (agent/deepseek {:capabilities fs}) (format "Create %s containing the word 'counted'." path))
        {:keys [requests prompt-tokens completion-tokens total-tokens] :as spent} (:usage result)]
    (is (true? (:done? result)))
    (is (= (:steps result) requests) "one usage block per model call")
    (is (pos? prompt-tokens))
    (is (pos? completion-tokens))
    (is (= total-tokens (+ prompt-tokens completion-tokens))
        "deepseek's total is the sum of the two")
    (testing "the summary and cost helpers work on a real run"
      (is (str/includes? (usage/summary spent) "tokens"))
      (is (pos? (usage/cost spent {:input 0.28 :output 0.42}))))))

;; ------------------------------------------------------ editing edn, for real

(deftest ^:integration the-model-edits-one-value-in-an-edn-file
  (let [dir (tmp-dir)
        file (str dir "/config.edn")]
    (spit file "{:server {:port 8080 :host \"localhost\"} :timeout-ms 5000 :debug false}\n")
    (let [result (agent/run (agent/deepseek {:capabilities [(caps/edn {:root dir :writable? true})]}) (format "In %s, change the server port to 9090. Leave everything else as it is." file))
          data (clojure.edn/read-string (slurp file))]
      (is (true? (:done? result)))
      (is (= 9090 (get-in data [:server :port])))
      (is (= "localhost" (get-in data [:server :host])))
      (is (= 5000 (:timeout-ms data)))
      (is (= false (:debug data))))))

(deftest ^:integration the-model-reads-a-value-out-of-an-edn-file
  (let [dir (tmp-dir)
        file (str dir "/deps.edn")]
    (spit file "{:deps {org.clojure/clojure {:mvn/version \"1.12.1\"}} :aliases {:test {:extra-paths [\"test\"]}}}\n")
    (let [result (agent/run (agent/deepseek {:capabilities [(caps/edn {:root dir})]}) (format "What version of org.clojure/clojure does %s depend on?" file))]
      (is (true? (:done? result)))
      (is (str/includes? (:output result) "1.12.1"))
      (is (= "{:deps {org.clojure/clojure {:mvn/version \"1.12.1\"}} :aliases {:test {:extra-paths [\"test\"]}}}\n"
             (slurp file)) "a read-only agent left the file exactly as it was"))))

;; ------------------------------------------------ a real research task, live

(deftest ^:integration describes-lithuania-using-wikipedia
  (let [searched (atom [])
        wikipedia (search/wikipedia {})
        capability (caps/search
                    {:id :wikipedia :tool-name "wikipedia_search"
                     :backend (fn [args] (swap! searched conj (:query args)) (wikipedia args))
                     :max-results 3})
        assistant (agent/deepseek {:capabilities [capability]})
        {:keys [output tool-calls done? usage] :as result}
        (agent/run assistant "describe lithuania using wikipedia")]
    (testing "it finished, having actually searched rather than answering from memory"
      (is (true? done?))
      (is (seq @searched) "the backend was called")
      (is (some #(= "wikipedia_search" (:tool %)) tool-calls))
      (is (every? #(str/includes? (str/lower-case %) "lithuania") @searched)
          "and searched for what was asked"))
    (testing "the results it was given really came from wikipedia"
      (let [results (str/join "\n" (map :result tool-calls))]
        (is (str/includes? results "wikipedia.org"))
        (is (str/includes? (str/lower-case results) "lithuania"))))
    (testing "and the answer is a description drawn from them"
      (let [lower (str/lower-case (str output))]
        (is (< 200 (count output)) "a description, not a one-liner")
        (is (some #(str/includes? lower %) ["vilnius" "baltic" "european union" "nato"])
            (str "expected some landmark fact, got: " output))))
    (testing "and it cost what a two-step run costs"
      (is (= (:steps result) (:requests usage)))
      (is (pos? (:total-tokens usage))))))

;; ------------------------------------------------------ a conversation, live

(deftest ^:integration a-session-remembers-across-turns
  (let [chat (agent/session
              (agent/deepseek {:capabilities [(caps/wikipedia {:max-results 3})]}))
        first-turn (agent/run chat "describe lithuania using wikipedia")
        ;; nothing here names Lithuania: the model can only answer from the transcript
        second-turn (agent/run chat "what is its capital? answer with the city name only")]
    (is (true? (:done? first-turn)))
    (is (true? (:done? second-turn)))
    (is (str/includes? (str/lower-case (:output second-turn)) "vilnius")
        (str "the follow-up had no subject of its own, got: " (:output second-turn)))
    (testing "the second turn continued the first rather than starting over"
      (is (= 1 (count (filter #(= "system" (:role %)) (agent/transcript chat)))))
      (is (< (count (:messages first-turn)) (count (agent/transcript chat)))))
    (testing "and the conversation's cost is the sum of its turns"
      (let [total (agent/session-usage chat)]
        (is (= (+ (:requests (:usage first-turn)) (:requests (:usage second-turn)))
               (:requests total)))
        (is (pos? (usage/cache-hit-rate total)) "a continued prefix is a cached prefix")))))

(deftest ^:integration a-session-can-be-cleared
  (let [chat (agent/session (agent/deepseek {}))]
    (agent/run chat "Remember the number 7. Reply with just: ok")
    (agent/clear! chat)
    (is (= [] (agent/transcript chat)))
    (let [after (agent/run chat "What number did I ask you to remember? Say 'none' if there was none.")]
      (is (str/includes? (str/lower-case (:output after)) "none")))))

;; ------------------------------------------------ the cache, against the api

(deftest ^:integration a-repeated-prompt-is-served-from-cache
  (let [assistant (agent/deepseek {:capabilities [(caps/filesystem)]})
        prompt "Reply with exactly one word: acknowledged"
        first-run (agent/run assistant prompt)
        second-run (agent/run assistant prompt)]
    (is (true? (:done? second-run)))
    (testing "the second send of an identical prompt is mostly cache"
      (is (pos? (:cache-hit-tokens (:usage second-run)))
          (str "no cache hits on a repeat: " (:usage second-run)))
      (is (>= (:cache-hit-tokens (:usage second-run))
              (:cache-hit-tokens (:usage first-run)))))))

(deftest ^:integration a-session-keeps-hitting-the-cache
  (let [chat (agent/session (agent/deepseek {}))
        turns (mapv #(agent/run chat %)
                    ["Name three Baltic countries, briefly."
                     "Which is largest by area?"
                     "And by population?"])
        hits (map #(:cache-hit-tokens (:usage %)) turns)]
    (is (every? true? (map :done? turns)))
    (testing "a growing transcript is a growing prefix, so hits never go backwards"
      (is (apply <= hits) (str "hits fell during the conversation: " hits)))
    (testing "and the conversation is served from cache, not resent in full"
      (is (pos? (last hits)) (str "no cache at all: " hits))
      (is (pos? (usage/cache-hit-rate (agent/session-usage chat)))))))

;; How MUCH gets cached is deliberately not asserted above. Deepseek caches in
;; blocks of 64 tokens, so three short turns can each report 128 hits while one
;; longer answer pushes the next turn to 256 - both were measured, at per-turn
;; rates from 0.38 to 0.84. The quantitative claims live in jj.bettong.cache-test,
;; where the bytes are ours and the numbers are deterministic.
