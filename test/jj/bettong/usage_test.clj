(ns jj.bettong.usage-test
  (:require [clojure.test :refer [deftest is testing]]
            [jj.bettong.usage :as usage]))

(def deepseek-block
  {:prompt_tokens 1200
   :completion_tokens 300
   :total_tokens 1500
   :prompt_cache_hit_tokens 1024
   :prompt_cache_miss_tokens 176
   :completion_tokens_details {:reasoning_tokens 90}})

(deftest normalizes-a-full-usage-block
  (is (= {:requests 1
          :prompt-tokens 1200
          :completion-tokens 300
          :total-tokens 1500
          :cache-hit-tokens 1024
          :cache-miss-tokens 176
          :reasoning-tokens 90}
         (usage/normalize deepseek-block))))

(deftest missing-fields-stay-zero
  (testing "a provider that reports only the basics"
    (is (= {:requests 1 :prompt-tokens 10 :completion-tokens 5 :total-tokens 15
            :cache-hit-tokens 0 :cache-miss-tokens 0 :reasoning-tokens 0}
           (usage/normalize {:prompt_tokens 10 :completion_tokens 5 :total_tokens 15}))))
  (testing "and one that reports nothing at all still counts as a request"
    (is (= (assoc usage/zero :requests 1) (usage/normalize nil)))
    (is (= (assoc usage/zero :requests 1) (usage/normalize {})))))

(deftest accepts-the-anthropic-style-names
  (is (= {:requests 1 :prompt-tokens 7 :completion-tokens 3 :total-tokens 0
          :cache-hit-tokens 4 :cache-miss-tokens 0 :reasoning-tokens 0}
         (usage/normalize {:input_tokens 7 :output_tokens 3 :cache_read_input_tokens 4}))))

(deftest adds-usage-across-steps
  (let [one (usage/normalize deepseek-block)]
    (is (= {:requests 2 :prompt-tokens 2400 :completion-tokens 600 :total-tokens 3000
            :cache-hit-tokens 2048 :cache-miss-tokens 352 :reasoning-tokens 180}
           (usage/add one one))))
  (testing "zero is the identity"
    (is (= (usage/normalize deepseek-block) (usage/add usage/zero (usage/normalize deepseek-block)))))
  (testing "nils are tolerated"
    (is (= usage/zero (usage/add nil nil)))))

(deftest reads-usage-off-a-message
  (let [message (with-meta {:role "assistant" :content "hi"} {:usage deepseek-block})]
    (is (= 1200 (:prompt-tokens (usage/of-message message)))))
  (testing "a stubbed chat-fn carries no metadata, but the request still counted"
    (is (= (assoc usage/zero :requests 1) (usage/of-message {:role "assistant"})))))

;; ---------------------------------------------------------------------- cost

(deftest computes-cost-from-prices
  (let [spent {:prompt-tokens 1000000 :completion-tokens 500000 :cache-hit-tokens 0}]
    (is (= 0.5 (usage/cost spent {:input 0.3 :output 0.4})))))

(deftest cached-prompt-tokens-are-billed-at-the-cache-price
  (let [spent {:prompt-tokens 1000000 :completion-tokens 0 :cache-hit-tokens 800000}]
    (testing "800k cached at 0.1 + 200k fresh at 1.0 = 0.28"
      (is (< (Math/abs (- 0.28 (usage/cost spent {:input 1.0 :output 2.0 :cache-hit 0.1}))) 1e-9))))
  (testing "without a cache price everything is billed as input"
    (let [spent {:prompt-tokens 1000000 :completion-tokens 0 :cache-hit-tokens 800000}]
      (is (= 1.0 (usage/cost spent {:input 1.0 :output 2.0}))))))

(deftest cost-is-nil-without-prices
  (is (nil? (usage/cost (usage/normalize deepseek-block) nil)))
  (is (nil? (usage/cost nil {:input 1.0 :output 1.0}))))

(deftest cost-of-nothing-is-nothing
  (is (zero? (usage/cost usage/zero {:input 1.0 :output 1.0}))))

;; ------------------------------------------------------------------- summary

(deftest reports-the-cache-hit-rate
  (is (= 0.8 (usage/cache-hit-rate {:prompt-tokens 1000 :cache-hit-tokens 800})))
  (is (= 0.0 (usage/cache-hit-rate {:prompt-tokens 1000 :cache-hit-tokens 0})))
  (testing "nothing sent yet, nothing to report"
    (is (nil? (usage/cache-hit-rate usage/zero)))
    (is (nil? (usage/cache-hit-rate {})))))

(deftest summarizes-a-run
  (is (= "2 requests 3000 tokens (2400 in, 600 out) [2048 cached, 85% of input]"
         (usage/summary (usage/add (usage/normalize deepseek-block) (usage/normalize deepseek-block))))))

(deftest summary-handles-one-request-and-no-cache
  (is (= "1 request 15 tokens (10 in, 5 out)"
         (usage/summary (usage/normalize {:prompt_tokens 10 :completion_tokens 5 :total_tokens 15}))))
  (testing "a provider that omits the total gets it added up"
    (is (= "1 request 15 tokens (10 in, 5 out)"
           (usage/summary (usage/normalize {:prompt_tokens 10 :completion_tokens 5}))))))
