(ns jj.bettong.capability
  "The capability protocol.

   An agent can do nothing on its own. Everything it can do - touch the
   filesystem, run a command, call your API - comes from a capability you hand
   to `jj.bettong.agent/run` explicitly. No capability, no tool, no way in.

   A capability is anything satisfying `Capability`. A plain map already does:

     {:id :weather
      :instructions \"Temperatures are in celsius.\"
      :tools [{:name \"get_weather\"
               :description \"Current weather for a city.\"
               :parameters {:city {:type \"string\" :description \"City name.\" :required true}}
               :handler (fn [{:keys [city]}] (str \"It is raining in \" city))}]}

   so does a defrecord or a reify, when you need state or a lifecycle."
  (:require [clojure.string :as str]
            [jj.bettong.secrets :as secrets]))

(defprotocol Capability
  (id [this]
    "A keyword or string naming this capability. Shown in events and errors.")
  (tools [this]
    "A seq of tool specs. Each is {:name :description :parameters :handler},
     where :handler is (fn [args] -> anything printable) and args is a map with
     keyword keys, decoded from what the model sent.")
  (instructions [this]
    "Extra system-prompt text telling the model how to use this capability,
     or nil."))

;; A plain map is a capability. This is the path of least resistance and the one
;; most users will want.
(extend-protocol Capability
  clojure.lang.IPersistentMap
  (id [m] (:id m))
  (tools [m] (:tools m))
  (instructions [m] (:instructions m)))

;; ------------------------------------------------------------- tool schemas

(defn parameters->json-schema
  "Tool parameters may be given as a ready-made JSON schema object, or as the
   shorthand {:city {:type \"string\" :description \"...\" :required true}}."
  [parameters]
  (cond
    (empty? parameters) {:type "object" :properties {} :required []}
    (= "object" (:type parameters)) parameters
    :else {:type "object"
           :properties (reduce-kv (fn [m k v] (assoc m k (dissoc v :required))) {} parameters)
           :required (->> parameters
                          (filter (comp :required val))
                          (map (comp name key))
                          vec)}))

(defn tool-schema
  "The JSON the API expects for one tool spec."
  [{:keys [name description parameters]}]
  {:type "function"
   :function (cond-> {:name name
                      :parameters (parameters->json-schema parameters)}
               description (assoc :description description))})

;; ----------------------------------------------------------------- registry

(defn- validate-tool! [capability-id {:keys [name handler] :as spec}]
  (when-not (and (string? name) (not (str/blank? name)))
    (throw (ex-info "A tool needs a non-blank string :name"
                    {:capability capability-id :tool spec})))
  (when-not (ifn? handler)
    (throw (ex-info (str "Tool " name " needs an :handler function")
                    {:capability capability-id :tool spec}))))

(defn registry
  "Flattens capabilities into name -> {:schema :handler :capability :order}.
   Two capabilities offering the same tool name is an error: the agent would
   have no way to tell which one you meant.

   `:order` records where each tool was declared, because the tool list is part
   of the prompt the provider caches. Keeping declaration order means adding a
   capability appends its tools instead of reshuffling the ones already there,
   so the cached prefix survives."
  [capabilities]
  (reduce
   (fn [acc capability]
     (let [cid (id capability)]
       (reduce
        (fn [acc spec]
          (validate-tool! cid spec)
          (let [tool-name (:name spec)]
            (when-let [existing (get-in acc [tool-name :capability])]
              (throw (ex-info (str "Two capabilities both provide the tool " tool-name)
                              {:tool tool-name :capabilities [existing cid]})))
            (assoc acc tool-name {:schema (tool-schema spec)
                                  :handler (:handler spec)
                                  :capability cid
                                  :order (count acc)})))
        acc
        (tools capability))))
   {}
   capabilities))

(defn schemas
  "The :tools payload for the API, in the order the capabilities declared them.
   A map's own order is arbitrary once it grows past a handful of keys, and an
   arbitrary order that changes when a capability is added would throw away the
   provider's cache of everything before the change."
  [registry]
  (->> (vals registry) (sort-by :order) (mapv :schema)))

(defn system-text
  "The capabilities' own instructions, ready to append to the system prompt."
  [capabilities]
  (let [lines (keep instructions capabilities)]
    (when (seq lines)
      (str/join "\n" lines))))

(def ^:dynamic *redact-secrets?*
  "Whether tool output is scrubbed of credential values before the model sees
   it. On by default; bind to false only if you mean to hand a secret over."
  true)

(defn invoke
  "Runs a tool. Always returns a string: a tool that throws is reported to the
   model so it can read the error and try something else, and a tool nobody
   granted is reported as exactly that.

   Whatever comes back is redacted, because every tool result is uploaded to the
   model provider - including the output of tools you wrote."
  [registry name args]
  (let [result (if-let [handler (get-in registry [name :handler])]
                 (try
                   (str (handler args))
                   (catch Exception e
                     (str "ERROR: " (or (.getMessage e) (str e)))))
                 (str "ERROR: no capability provides a tool called " name))]
    (cond-> result *redact-secrets?* secrets/redact)))
