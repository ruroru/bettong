(ns jj.bettong.llm-test
  (:require [jj.bettong.impl.json :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [jj.bettong.llm :as llm]
            [jj.bettong.test-server :as ts]))

(deftest parse-response-extracts-the-assistant-message
  (let [body (json/write-str
              {:choices [{:message {:role "assistant" :content "hello"}}]
               :usage {:total_tokens 42}
               :model "deepseek-v4-flash"})
        message (llm/parse-response body)]
    (is (= {:role "assistant" :content "hello"} message))
    (is (= {:total_tokens 42} (:usage (meta message))))
    (is (= "deepseek-v4-flash" (:model (meta message))))))

(deftest parse-response-keeps-tool-calls
  (let [body (json/write-str
              {:choices [{:message {:role "assistant" :content nil
                                    :tool_calls [{:id "call_1" :type "function"
                                                  :function {:name "write_file"
                                                             :arguments "{\"path\":\"/tmp/foo.txt\"}"}}]}}]})
        message (llm/parse-response body)]
    (is (= "call_1" (get-in message [:tool_calls 0 :id])))
    (is (= "write_file" (get-in message [:tool_calls 0 :function :name])))
    (testing "arguments stay a raw JSON string, as the API sends them"
      (is (string? (get-in message [:tool_calls 0 :function :arguments]))))))

(deftest parse-response-rejects-an-empty-choice-list
  (is (thrown-with-msg? Exception #"No message in API response"
                        (llm/parse-response (json/write-str {:choices []})))))

(deftest api-key-prefers-the-argument
  (is (= "explicit" (llm/api-key "explicit"))))

(deftest api-key-falls-back-to-the-environment
  (if (System/getenv "DEEPSEEK_API_KEY")
    (is (= (System/getenv "DEEPSEEK_API_KEY") (llm/api-key nil)))
    (is (thrown-with-msg? Exception #"No DeepSeek API key" (llm/api-key nil)))))

(deftest defaults-point-at-deepseek
  (is (= "https://api.deepseek.com" llm/default-base-url))
  (is (= "deepseek-v4-flash" llm/default-model)))

;; ============================================================================
;; The client against a real HTTP server, so the request it actually sends is
;; checked rather than assumed.
;; ============================================================================

(def ^:private completion
  (json/write-str {:choices [{:message {:role "assistant" :content "hello"}}]
                         :usage {:total_tokens 11}}))

(deftest chat-posts-what-the-api-expects
  (ts/with-server [s (ts/json-response completion)]
    (let [message (llm/chat {:base-url (:base-url s)
                             :api-key "sk-test"
                             :model "deepseek-v4-pro"
                             :temperature 0.2
                             :messages [{:role "user" :content "hi"}]
                             :tools [{:type "function" :function {:name "noop" :parameters {:type "object"}}}]})
          request (ts/only-request s)
          body (json/read-str (:body request))]
      (is (= :post (:request-method request)))
      (is (= "/chat/completions" (:uri request)))
      (is (= "Bearer sk-test" (get-in request [:headers "authorization"])))
      (is (str/starts-with? (get-in request [:headers "content-type"]) "application/json"))
      (is (= "deepseek-v4-pro" (:model body)))
      (is (= [{:role "user" :content "hi"}] (:messages body)))
      (is (= 0.2 (:temperature body)))
      (is (= "noop" (get-in body [:tools 0 :function :name])))
      (testing "tool_choice is only sent alongside tools"
        (is (= "auto" (:tool_choice body))))
      (is (= {:role "assistant" :content "hello"} message)))))

(deftest chat-omits-tools-when-there-are-none
  (ts/with-server [s (ts/json-response completion)]
    (llm/chat {:base-url (:base-url s) :api-key "k" :messages [{:role "user" :content "hi"}] :tools []})
    (let [body (json/read-str (:body (ts/only-request s)))]
      (is (not (contains? body :tools)))
      (is (not (contains? body :tool_choice)))
      (testing "and the default model is used"
        (is (= llm/default-model (:model body)))))))

(deftest chat-passes-tool-calls-back-through
  (ts/with-server [s (ts/json-response
                      (json/write-str
                       {:choices [{:message {:role "assistant" :content nil
                                             :tool_calls [{:id "call_1" :type "function"
                                                           :function {:name "write_file"
                                                                      :arguments "{\"path\":\"/tmp/x\"}"}}]}}]}))]
    (let [message (llm/chat {:base-url (:base-url s) :api-key "k" :messages []})]
      (is (= "call_1" (get-in message [:tool_calls 0 :id])))
      (is (= "write_file" (get-in message [:tool_calls 0 :function :name]))))))

(deftest an-api-error-reaches-the-caller
  (ts/with-server [s {:status 401 :body (json/write-str {:error {:message "Authentication Fails"}})}]
    (is (thrown? Exception (llm/chat {:base-url (:base-url s) :api-key "bad" :messages []})))))
