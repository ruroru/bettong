(ns jj.bettong.usage
  "Token accounting. Every model response carries a usage block; the agent adds
   them up across the run so you can see what a task cost."
  (:require [clojure.string :as str]))

(def zero
  {:requests 0
   :prompt-tokens 0
   :completion-tokens 0
   :total-tokens 0
   :cache-hit-tokens 0
   :cache-miss-tokens 0
   :reasoning-tokens 0})

(defn normalize
  "Turns an API usage block into our keys. DeepSeek reports cache hits and misses
   separately (a hit is billed cheaper); other OpenAI-compatible providers omit
   them, and missing fields simply stay zero."
  [usage]
  (if (nil? usage)
    (assoc zero :requests 1)
    (let [n (fn [& ks] (or (some #(get usage %) ks) 0))]
      {:requests 1
       :prompt-tokens (n :prompt_tokens :input_tokens)
       :completion-tokens (n :completion_tokens :output_tokens)
       :total-tokens (n :total_tokens)
       :cache-hit-tokens (n :prompt_cache_hit_tokens :cache_read_input_tokens)
       :cache-miss-tokens (n :prompt_cache_miss_tokens)
       :reasoning-tokens (or (get-in usage [:completion_tokens_details :reasoning_tokens]) 0)})))

(defn add
  "Sums two usage maps field by field."
  [a b]
  (merge-with + (or a zero) (or b zero)))

(defn of-message
  "The usage `jj.bettong.llm/chat` stashed on the message it returned. A stubbed
   chat-fn carries none, which still counts as one request."
  [message]
  (normalize (:usage (meta message))))

(defn cost
  "What a run cost, given prices in currency per million tokens:

     {:input 0.28 :output 0.42}                  ; flat
     {:input 0.28 :output 0.42 :cache-hit 0.028} ; cheaper cached prompt tokens

   Prices change and differ per model, so the harness holds none - pass your
   provider's current numbers. Returns nil when no prices are given."
  [usage prices]
  (when (and usage prices)
    (let [{:keys [input output cache-hit]} prices
          {:keys [prompt-tokens completion-tokens cache-hit-tokens]} usage
          per-million (fn [tokens price] (if price (* (/ (double tokens) 1e6) (double price)) 0.0))
          cached (if cache-hit (min cache-hit-tokens prompt-tokens) 0)
          uncached (- prompt-tokens cached)]
      (+ (per-million uncached input)
         (per-million cached cache-hit)
         (per-million completion-tokens output)))))

(defn cache-hit-rate
  "The share of prompt tokens the provider served from its cache, 0.0 to 1.0.
   nil when nothing has been sent yet."
  [{:keys [prompt-tokens cache-hit-tokens]}]
  (when (and prompt-tokens (pos? prompt-tokens))
    (/ (double (or cache-hit-tokens 0)) prompt-tokens)))

(defn summary
  "One line worth printing at the end of a run."
  [{:keys [requests prompt-tokens completion-tokens total-tokens cache-hit-tokens] :as usage}]
  (str/join " " (cond-> [(format "%d request%s" requests (if (= 1 requests) "" "s"))
                         (format "%d tokens (%d in, %d out)"
                                 (if (pos? total-tokens) total-tokens (+ prompt-tokens completion-tokens))
                                 prompt-tokens
                                 completion-tokens)]
                  (pos? (or cache-hit-tokens 0))
                  (conj (format "[%d cached, %.0f%% of input]"
                                cache-hit-tokens
                                (* 100 (cache-hit-rate usage)))))))
