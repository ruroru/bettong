(ns jj.bettong.agent
  "The agent loop: prompt in, tool calls executed, final answer out.

   The agent starts with no abilities at all. What it can do is exactly what you
   pass in :capabilities - see `jj.bettong.capability`.

     (require '[jj.bettong.agent :as agent]
              '[jj.bettong.capabilities :as caps])

     (def assistant (agent/deepseek {:capabilities [(caps/filesystem)]}))
     (agent/run assistant \"Create file /tmp/foo.txt with content 'foo'\")"
  (:require [jj.bettong.impl.json :as json]
            [clojure.string :as str]
            [jj.bettong.capability :as cap]
            [jj.bettong.llm :as llm]
            [jj.bettong.usage :as usage]))

(def base-system-prompt
  (str "You are a capable agent running on the user's machine.\n"
       "Use the tools you have been given to actually carry out the user's request - "
       "do not just describe what you would do.\n"
       "Paths given by the user are real filesystem paths; use them exactly as given.\n"
       "If you have no tool for part of the request, say so plainly instead of pretending it is done.\n"
       "When the task is complete, reply with a short confirmation of what you did and stop calling tools."))

(def default-max-steps 10)

(defn approve-all
  "The default approval hook: every tool call is allowed."
  [_] true)

(defn ask-in-terminal
  "An :approve-fn that prints the tool call and waits for y/n on stdin.
   Anything other than y/yes denies the call."
  [{:keys [tool args]}]
  (println (format "Allow %s %s ? [y/N]" tool (pr-str args)))
  (flush)
  (let [answer (str/lower-case (str/trim (or (read-line) "")))]
    (if (#{"y" "yes"} answer)
      true
      {:allowed? false :reason "denied by the user"})))

(defn system-prompt
  "The base prompt plus whatever the granted capabilities want the model to know."
  [capabilities override]
  (let [base (or override base-system-prompt)]
    (if-let [extra (cap/system-text capabilities)]
      (str base "\n\n" extra)
      base)))

(defn parse-arguments
  "Tool-call arguments arrive as a JSON string; decode to a keyword map."
  [arguments]
  (cond
    (map? arguments) arguments
    (nil? arguments) {}
    (and (string? arguments) (str/blank? arguments)) {}
    :else (try
            (json/read-str arguments)
            (catch Exception e
              ;; This message goes straight back to the model, so it says what to fix.
              (throw (ex-info (str "Tool call arguments were not valid JSON (" (.getMessage e) "). "
                                   "Send arguments as JSON: strings double-quoted, array elements "
                                   "separated by commas.")
                              {:arguments arguments} e))))))

(defn- tool-result-message [tool-call content]
  {:role "tool"
   :tool_call_id (:id tool-call)
   :name (get-in tool-call [:function :name])
   :content content})

(defn- approval-error
  "Runs the approval hook. Returns nil to allow, or the denial message.
   Anything truthy allows - so an allowlist can just be a set - while false, nil
   or {:allowed? false :reason <string>} denies."
  [approve-fn call]
  (let [verdict (try
                  (approve-fn call)
                  (catch Exception e {:allowed? false
                                      :reason (str "approval hook threw: " (.getMessage e))}))
        denied? (if (map? verdict) (false? (:allowed? verdict)) (not verdict))
        reason (when (map? verdict) (:reason verdict))]
    (when denied?
      (str "ERROR: " (:tool call) " was not allowed" (when reason (str ": " reason))))))

(defn- execute-tool-call [registry approve-fn on-event tool-call]
  (let [name (get-in tool-call [:function :name])
        args (try
               (parse-arguments (get-in tool-call [:function :arguments]))
               (catch Exception e {::error (.getMessage e)}))
        capability (get-in registry [name :capability])
        call {:tool name :args args :capability capability}
        result (or (some->> (::error args) (str "ERROR: "))
                   (approval-error approve-fn call)
                   (cap/invoke registry name args))
        record (assoc call :result result)]
    (on-event (assoc record :event :tool-call))
    {:message (tool-result-message tool-call result)
     :record record}))

(defprotocol Agent
  "Something that can run a prompt to completion.

   Implement it yourself when you want a stand-in - a canned agent for your own
   tests, one that caches, one that routes between models."
  (run [this prompt] [this prompt opts]
    "Runs `prompt` and returns
       {:output <final assistant text>
        :steps <number of model calls>
        :tool-calls [{:tool .. :capability .. :args .. :result ..} ...]
        :messages <full transcript>
        :usage {:requests .. :prompt-tokens .. :completion-tokens .. :total-tokens
                :cache-hit-tokens .. :cache-miss-tokens .. :reasoning-tokens ..}
        :done? <false if it hit :max-steps>}

     `opts` overrides the agent's own configuration for this one call."))

(defn- opening-messages
  "The transcript a turn starts from. Continuing a conversation means appending
   to what is already there - the system prompt is not repeated, and the prefix
   the provider has cached stays intact."
  [prior capabilities system prompt]
  (if (seq prior)
    (conj (vec prior) {:role "user" :content prompt})
    [{:role "system" :content (system-prompt capabilities system)}
     {:role "user" :content prompt}]))

(defn- execute
  [{:keys [capabilities api-key model base-url max-steps system
           chat-fn on-event temperature approve-fn messages]}
   prompt]
  (let [registry (cap/registry capabilities)
        chat-fn (or chat-fn llm/chat)
        on-event (or on-event (constantly nil))
        approve-fn (or approve-fn approve-all)
        max-steps (or max-steps default-max-steps)
        schemas (cap/schemas registry)]
    (loop [messages (opening-messages messages capabilities system prompt)
           calls []
           spent usage/zero
           step 0]
      (if (>= step max-steps)
        {:output nil :steps step :tool-calls calls :messages messages :usage spent :done? false}
        (let [message (chat-fn {:messages messages
                                :tools schemas
                                :model model
                                :base-url base-url
                                :api-key api-key
                                :temperature temperature})
              spent (usage/add spent (usage/of-message message))
              messages (conj messages message)
              step (inc step)]
          (on-event {:event :assistant :step step :message message
                     :usage (usage/of-message message)})
          (if-let [tool-calls (seq (:tool_calls message))]
            (let [results (mapv #(execute-tool-call registry approve-fn on-event %) tool-calls)]
              (recur (into messages (map :message results))
                     (into calls (map :record results))
                     spent
                     step))
            {:output (:content message)
             :steps step
             :tool-calls calls
             :messages messages
             :usage spent
             :done? true}))))))

(def config-keys
  "Everything an agent accepts, at construction or as a per-call override."
  #{:capabilities :api-key :model :base-url :max-steps :system
    :approve-fn :on-event :temperature :chat-fn :messages})

(defn- check-config! [config where]
  (when-let [unknown (seq (remove config-keys (keys config)))]
    ;; a silently ignored typo means an agent with no tools and no explanation
    (throw (ex-info (format "Unknown %s option%s: %s. Known options: %s"
                            where
                            (if (= 1 (count unknown)) "" "s")
                            (str/join ", " (sort (map str unknown)))
                            (str/join ", " (sort (map str config-keys))))
                    {:unknown (set unknown) :known config-keys}))))

(defrecord LlmAgent [config]
  Agent
  (run [this prompt] (run this prompt nil))
  (run [_ prompt opts]
    (check-config! opts "override")
    (execute (merge config opts) prompt)))

(defn deepseek
  "An agent backed by DeepSeek's API. Configure it once, run it many times:

     (def assistant
       (agent/deepseek {:api-key (System/getenv \"DEEPSEEK_API_KEY\")
                        :model \"deepseek-v4-flash\"
                        :capabilities [(caps/filesystem {:root \"/srv/data\"})]}))

     (agent/run assistant \"Summarise report.txt\")
     (agent/run assistant \"And again in French\" {:max-steps 3})   ; per-call override

   Options:
     :capabilities  what the agent may do. A seq of anything satisfying
                    `jj.bettong.capability/Capability`, including plain maps. Empty
                    by default: with no capabilities it has no tools and can only
                    talk back.
     :api-key       defaults to the DEEPSEEK_API_KEY environment variable
     :model         defaults to deepseek-v4-flash
     :base-url      defaults to https://api.deepseek.com - point it at any
                    OpenAI-compatible endpoint
     :max-steps     model calls before giving up (default 10)
     :system        replace the base system prompt; capability instructions are
                    still appended to it
     :approve-fn    (fn [{:keys [tool args capability]}]) -> anything truthy allows
                    the call; false/nil, or {:allowed? false :reason <string>},
                    denies it and hands the reason to the model
     :on-event      fn called with progress maps
     :temperature   passed through to the model
     :chat-fn       override the LLM call itself, for testing"
  ([] (deepseek {}))
  ([config]
   (check-config! config "agent")
   (->LlmAgent config)))

;; ================================================================= sessions

(defrecord Session [agent history spent]
  Agent
  (run [this prompt] (run this prompt nil))
  (run [_ prompt opts]
    ;; one turn at a time: a turn reads the transcript, waits on the model, then
    ;; writes it back, and two of those interleaved would lose a turn
    (locking history
      (let [result (run agent prompt (assoc opts :messages @history))]
        (reset! history (:messages result))
        (swap! spent usage/add (:usage result))
        result))))

(defn session
  "An agent that remembers. Each turn continues the transcript instead of
   starting one, so follow-ups work and the provider's cache keeps paying:

     (def chat (agent/session assistant))
     (agent/run chat \"Describe Lithuania using wikipedia\")
     (agent/run chat \"Now in one sentence\")      ; knows what 'it' is

   A session is itself an `Agent`, so anything that takes one takes this."
  [agent]
  (->Session agent (atom []) (atom usage/zero)))

(defn transcript
  "Everything said so far, as the messages the API sees."
  [session]
  @(:history session))

(defn session-usage
  "What the conversation has cost in total, across every turn."
  [session]
  @(:spent session))

(defn clear!
  "Forgets the conversation, keeping the agent. The next turn starts fresh."
  [session]
  (reset! (:history session) [])
  (reset! (:spent session) usage/zero)
  session)
