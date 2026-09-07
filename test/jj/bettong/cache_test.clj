(ns jj.bettong.cache-test
  "Providers cache a prompt by its prefix: the request is served from cache up to
   the first byte that differs from one they have seen. Nothing here can test
   their cache - what it tests is the half we control, that the bytes we send in
   front are the same bytes as last time."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.agent :as agent]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.impl.json :as json]
            [jj.bettong.test-server :as ts]
            [jj.bettong.usage :as usage]))

(defn- answering [content]
  (fn [_] (ts/json-response (json/write-str {:choices [{:message {:role "assistant" :content content}}]}))))

(defn- bodies [server]
  (mapv #(json/read-str (:body %)) (ts/requests server)))

(defn- raw-tools
  "The tools array as it goes on the wire, as text - the thing the provider
   hashes, rather than a Clojure value that might compare equal while
   serialising differently."
  [body]
  (json/write-str (:tools body)))

(def ^:private everything
  [(caps/filesystem {:writable? true}) (caps/shell) (caps/edn {:writable? true})])

;; ------------------------------------------------------- a stable front end

(deftest two-runs-of-one-agent-send-the-same-prefix
  (ts/with-server [llm (answering "ok")]
    (let [assistant (agent/deepseek {:base-url (:base-url llm) :api-key "k" :capabilities everything})]
      (agent/run assistant "first question")
      (agent/run assistant "second question")
      (let [[a b] (bodies llm)]
        (testing "the system message is byte for byte the same"
          (is (= (first (:messages a)) (first (:messages b))))
          (is (= (json/write-str (first (:messages a)))
                 (json/write-str (first (:messages b))))))
        (testing "and so is the tool list"
          (is (= (raw-tools a) (raw-tools b))))
        (testing "only the user's own message differs"
          (is (not= (second (:messages a)) (second (:messages b)))))))))

(deftest the-tool-list-is-serialised-identically-every-time
  (testing "a map big enough to lose insertion order must still encode the same"
    (ts/with-server [llm (answering "ok")]
      (let [assistant (agent/deepseek {:base-url (:base-url llm) :api-key "k" :capabilities everything})]
        (dotimes [_ 3] (agent/run assistant "x"))
        (let [encodings (map raw-tools (bodies llm))]
          (is (= 1 (count (distinct encodings))))
          (is (= 8 (count (:tools (first (bodies llm)))))))))))

(deftest adding-a-capability-leaves-the-earlier-tools-untouched
  (testing "so the prefix up to the new tool still matches what was cached"
    (ts/with-server [llm (answering "ok")]
      (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                  :capabilities [(caps/filesystem {:writable? true}) (caps/shell)]})
                 "x")
      (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                  :capabilities [(caps/filesystem {:writable? true}) (caps/shell) (caps/edn)]})
                 "x")
      (let [[before after] (map :tools (bodies llm))]
        (is (= before (vec (butlast after))))
        (is (= "read_edn" (get-in (last after) [:function :name])))
        (testing "as text, which is what actually gets hashed"
          (is (str/starts-with? (json/write-str after)
                                (subs (json/write-str before) 0 (dec (count (json/write-str before)))))))))))

(deftest capability-order-is-the-order-on-the-wire
  (ts/with-server [llm (answering "ok")]
    (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                :capabilities [(caps/edn) (caps/shell) (caps/filesystem {:writable? true})]})
               "x")
    (is (= ["read_edn" "run_shell" "read_file" "list_dir" "write_file" "delete_file"]
           (map #(get-in % [:function :name]) (:tools (first (bodies llm))))))))

;; -------------------------------------------------- what a session is worth

(deftest a-session-turn-extends-the-previous-turn-exactly
  (testing "the whole of turn one is the start of turn two, so all of it is cacheable"
    (ts/with-server [llm (answering "an answer")]
      (let [chat (agent/session (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                                 :capabilities [(caps/filesystem {:writable? true})]}))]
        (agent/run chat "first")
        (agent/run chat "second")
        (agent/run chat "third")
        (let [[a b c] (map :messages (bodies llm))]
          (is (= a (subvec b 0 (count a))))
          (is (= b (subvec c 0 (count b))))
          (testing "each turn adds exactly the previous answer and the new question"
            (is (= (+ 2 (count a)) (count b)))
            (is (= (+ 2 (count b)) (count c)))))))))

(deftest without-a-session-every-run-presents-a-new-prompt
  (testing "the system prompt still matches, but nothing after it does"
    (ts/with-server [llm (answering "ok")]
      (let [assistant (agent/deepseek {:base-url (:base-url llm) :api-key "k"})]
        (agent/run assistant "first")
        (agent/run assistant "second")
        (let [[a b] (map :messages (bodies llm))]
          (is (= (first a) (first b)))
          (is (= 2 (count a) (count b)) "no history carried over"))))))

;; ------------------------------------------------------------- the hazards

(deftest instructions-that-vary-per-run-poison-the-prefix
  (testing "a root path baked into a capability's instructions changes the system
            message, and with it every token after it"
    (ts/with-server [llm (answering "ok")]
      (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                  :capabilities [(caps/filesystem {:root "/tmp/run-1"})]})
                 "x")
      (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k"
                                  :capabilities [(caps/filesystem {:root "/tmp/run-2"})]})
                 "x")
      (let [[a b] (map #(first (:messages %)) (bodies llm))]
        (is (not= a b) "which is the cost of putting a per-run value in a prompt")
        (is (str/includes? (:content a) "/tmp/run-1"))))))

(deftest a-changed-system-prompt-changes-everything
  (ts/with-server [llm (answering "ok")]
    (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :system "be terse"}) "x")
    (agent/run (agent/deepseek {:base-url (:base-url llm) :api-key "k" :system "be verbose"}) "x")
    (let [[a b] (map #(first (:messages %)) (bodies llm))]
      (is (not= a b)))))

;; ----------------------------------------------------------- the accounting

(deftest cache-hits-are-added-up-over-a-session
  (let [turns (atom [[{:role "assistant" :content "a"}
                      {:prompt_tokens 1000 :completion_tokens 10 :total_tokens 1010
                       :prompt_cache_hit_tokens 0 :prompt_cache_miss_tokens 1000}]
                     [{:role "assistant" :content "b"}
                      {:prompt_tokens 1200 :completion_tokens 10 :total_tokens 1210
                       :prompt_cache_hit_tokens 960 :prompt_cache_miss_tokens 240}]])]
    (ts/with-server [llm (fn [_] (let [[message usage] (ffirst (swap-vals! turns rest))]
                                   (ts/json-response (json/write-str {:choices [{:message message}]
                                                                      :usage usage}))))]
      (let [chat (agent/session (agent/deepseek {:base-url (:base-url llm) :api-key "k"}))
            first-turn (agent/run chat "one")
            second-turn (agent/run chat "two")
            total (agent/session-usage chat)]
        (testing "the first turn has nothing to hit"
          (is (= 0.0 (usage/cache-hit-rate (:usage first-turn)))))
        (testing "the second is mostly a prefix the provider has already seen"
          (is (= 0.8 (usage/cache-hit-rate (:usage second-turn)))))
        (is (= 960 (:cache-hit-tokens total)))
        (is (= 1240 (:cache-miss-tokens total)))
        (testing "and the conversation's rate sits between its turns"
          (is (< 0.4 (usage/cache-hit-rate total) 0.5)))))))

(deftest what-the-cache-is-worth-in-money
  (let [prices {:input 0.22 :output 0.66 :cache-hit 0.007}
        spent {:prompt-tokens 17579 :completion-tokens 1716 :cache-hit-tokens 15744}
        uncached (usage/cost (assoc spent :cache-hit-tokens 0) prices)
        cached (usage/cost spent prices)]
    (is (< cached uncached))
    (testing "90% of input served from cache takes about two thirds off the bill"
      (is (< 0.6 (- 1 (/ cached uncached)) 0.7)))))
