(ns jj.bettong.llm
  "Thin client for the DeepSeek chat-completions API (OpenAI-compatible)."
  (:require [jj.bettong.impl.json :as json]
            [jj.potoroo.httpclient :as http]))

(def default-base-url "https://api.deepseek.com")
(def default-model "deepseek-v4-flash")

(defn api-key
  "The API key to use, from the argument or the DEEPSEEK_API_KEY env var."
  [k]
  (or k
      (System/getenv "DEEPSEEK_API_KEY")
      (throw (ex-info "No DeepSeek API key: pass :api-key or set DEEPSEEK_API_KEY" {}))))

(defn- error-message
  "The provider's own words for what went wrong, when it gives any."
  [body]
  (let [parsed (try (json/read-str body) (catch Exception _ nil))]
    (or (get-in parsed [:error :message])
        (:message parsed)
        (when body (subs body 0 (min 200 (count body)))))))

(defn parse-response
  "Pulls the assistant message out of a chat-completions response body."
  [body]
  (let [parsed (json/read-str body)
        message (get-in parsed [:choices 0 :message])]
    (when-not message
      (throw (ex-info "No message in API response" {:body parsed})))
    (with-meta message {:usage (:usage parsed) :model (:model parsed)})))

(defn chat
  "Sends `messages` (and optional `tools`) to the model, returns the assistant
   message map: {:role \"assistant\" :content ... :tool_calls [...]}."
  [{:keys [messages tools model base-url api-key timeout-ms temperature]}]
  (let [url (str (or base-url default-base-url) "/chat/completions")
        payload (cond-> {:model (or model default-model)
                         :messages messages}
                  (seq tools) (assoc :tools tools :tool_choice "auto")
                  temperature (assoc :temperature temperature))
        {:keys [status body]}
        (http/request {:method :post
                       :url url
                       :headers {"Authorization" (str "Bearer " (jj.bettong.llm/api-key api-key))
                                 "Content-Type" "application/json"
                                 "Accept" "application/json"}
                       :body (json/write-str payload)
                       :timeout-ms (or timeout-ms 120000)})]
    ;; The JDK client returns a 4xx/5xx like any other response, so the check is ours.
    (when-not (<= 200 status 299)
      (throw (ex-info (format "Model request failed (HTTP %d): %s" status (error-message body))
                      {:status status :url url})))
    (parse-response body)))
