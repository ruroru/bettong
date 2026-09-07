(ns jj.bettong.capability-test
  (:require [clojure.test :refer [deftest is testing]]
            [jj.bettong.capability :as cap]))

;; ------------------------------------------------- the three ways to make one

(def map-capability
  {:id :weather
   :instructions "Temperatures are in celsius."
   :tools [{:name "get_weather"
            :description "Current weather for a city."
            :parameters {:city {:type "string" :description "City name." :required true}}
            :handler (fn [{:keys [city]}] (str "raining in " city))}]})

(defrecord CounterCapability [counter]
  cap/Capability
  (id [_] :counter)
  (instructions [_] nil)
  (tools [_] [{:name "bump"
               :description "Increment the counter."
               :handler (fn [_] (swap! counter inc))}]))

(defn reify-capability [log]
  (reify cap/Capability
    (id [_] :logger)
    (instructions [_] "Log anything noteworthy.")
    (tools [_] [{:name "log"
                 :description "Append a line to the log."
                 :parameters {:line {:type "string" :required true}}
                 :handler (fn [{:keys [line]}] (swap! log conj line) "logged")}])))

(deftest a-plain-map-is-a-capability
  (is (= :weather (cap/id map-capability)))
  (is (= "Temperatures are in celsius." (cap/instructions map-capability)))
  (is (= 1 (count (cap/tools map-capability)))))

(deftest a-record-is-a-capability
  (let [c (->CounterCapability (atom 0))]
    (is (= :counter (cap/id c)))
    (is (nil? (cap/instructions c)))
    (is (= "bump" (:name (first (cap/tools c)))))))

(deftest a-reify-is-a-capability
  (is (= :logger (cap/id (reify-capability (atom []))))))

(deftest capabilities-can-hold-state
  (let [counter (atom 0)
        registry (cap/registry [(->CounterCapability counter)])]
    (cap/invoke registry "bump" {})
    (cap/invoke registry "bump" {})
    (is (= 2 @counter))))

;; ------------------------------------------------------------------ schemas

(deftest shorthand-parameters-become-a-json-schema
  (is (= {:type "object"
          :properties {:city {:type "string" :description "City name."}}
          :required ["city"]}
         (cap/parameters->json-schema {:city {:type "string" :description "City name." :required true}}))))

(deftest optional-parameters-are-not-required
  (let [schema (cap/parameters->json-schema {:a {:type "string" :required true}
                                             :b {:type "string"}})]
    (is (= ["a"] (:required schema)))
    (is (= #{:a :b} (set (keys (:properties schema)))))
    (testing ":required is stripped from the property itself"
      (is (= {:type "string"} (get-in schema [:properties :a]))))))

(deftest a-full-json-schema-passes-through-untouched
  (let [raw {:type "object"
             :properties {:xs {:type "array" :items {:type "number"}}}
             :required ["xs"]}]
    (is (= raw (cap/parameters->json-schema raw)))))

(deftest no-parameters-means-an-empty-object
  (is (= {:type "object" :properties {} :required []} (cap/parameters->json-schema nil)))
  (is (= {:type "object" :properties {} :required []} (cap/parameters->json-schema {}))))

(deftest tool-schema-has-the-shape-the-api-wants
  (is (= {:type "function"
          :function {:name "get_weather"
                     :description "Current weather for a city."
                     :parameters {:type "object"
                                  :properties {:city {:type "string" :description "City name."}}
                                  :required ["city"]}}}
         (cap/tool-schema (first (cap/tools map-capability))))))

(deftest a-tool-without-a-description-still-works
  (is (= {:name "ping" :parameters {:type "object" :properties {} :required []}}
         (:function (cap/tool-schema {:name "ping" :handler (fn [_] "pong")})))))

;; ----------------------------------------------------------------- registry

(deftest registry-merges-capabilities
  (let [registry (cap/registry [map-capability (reify-capability (atom []))])]
    (is (= #{"get_weather" "log"} (set (keys registry))))
    (is (= :weather (get-in registry ["get_weather" :capability])))
    (is (= :logger (get-in registry ["log" :capability])))))

(deftest an-empty-capability-list-yields-no-tools
  (is (= {} (cap/registry [])))
  (is (= {} (cap/registry nil)))
  (is (= [] (cap/schemas (cap/registry [])))))

(deftest duplicate-tool-names-are-rejected
  (let [other (assoc map-capability :id :other)]
    (is (thrown-with-msg? Exception #"Two capabilities both provide the tool get_weather"
                          (cap/registry [map-capability other])))))

(deftest malformed-tools-are-rejected-at-build-time
  (testing "a missing name"
    (is (thrown-with-msg? Exception #"non-blank string :name"
                          (cap/registry [{:id :bad :tools [{:handler (fn [_])}]}]))))
  (testing "a blank name"
    (is (thrown-with-msg? Exception #"non-blank string :name"
                          (cap/registry [{:id :bad :tools [{:name "  " :handler (fn [_])}]}]))))
  (testing "a missing handler"
    (is (thrown-with-msg? Exception #"needs an :handler function"
                          (cap/registry [{:id :bad :tools [{:name "x"}]}]))))
  (testing "the error says which capability is at fault"
    (is (= :bad (:capability (ex-data (try (cap/registry [{:id :bad :tools [{:name "x"}]}])
                                           (catch Exception e e))))))))

;; ------------------------------------------------------------------- invoke

(deftest invoke-passes-decoded-args-to-the-handler
  (is (= "raining in Vilnius"
         (cap/invoke (cap/registry [map-capability]) "get_weather" {:city "Vilnius"}))))

(deftest invoke-stringifies-whatever-the-handler-returns
  (is (= "1" (cap/invoke (cap/registry [(->CounterCapability (atom 0))]) "bump" {}))))

(deftest an-ungranted-tool-says-so
  (testing "this is what the model sees when the user withheld a capability"
    (is (= "ERROR: no capability provides a tool called write_file"
           (cap/invoke (cap/registry [map-capability]) "write_file" {:path "/tmp/x"})))
    (is (= "ERROR: no capability provides a tool called anything"
           (cap/invoke (cap/registry []) "anything" {})))))

(deftest a-throwing-handler-becomes-an-error-string
  (let [registry (cap/registry [{:id :bomb
                                 :tools [{:name "boom"
                                          :handler (fn [_] (throw (ex-info "kaboom" {})))}]}])]
    (is (= "ERROR: kaboom" (cap/invoke registry "boom" {})))))

;; ------------------------------------------------------------- instructions

(deftest system-text-collects-instructions
  (is (= "Temperatures are in celsius.\nLog anything noteworthy."
         (cap/system-text [map-capability (reify-capability (atom []))]))))

(deftest system-text-skips-capabilities-with-nothing-to-say
  (is (= "Temperatures are in celsius."
         (cap/system-text [map-capability (->CounterCapability (atom 0))])))
  (is (nil? (cap/system-text [])))
  (is (nil? (cap/system-text [(->CounterCapability (atom 0))]))))

;; ------------------------------------------------------- order and the cache

(defn- tool-names [capabilities]
  (mapv #(get-in % [:function :name]) (cap/schemas (cap/registry capabilities))))

(def ^:private many
  "Enough tools that the registry map is no longer small enough to keep
   insertion order on its own - which is exactly when this used to break."
  (mapv (fn [i]
          {:id (keyword (str "cap" i))
           :tools [{:name (str "tool_" i) :handler (fn [_] i)}]})
        (range 12)))

(deftest tools-are-sent-in-declaration-order
  (is (= (mapv #(str "tool_" %) (range 12)) (tool-names many))
      "not the arbitrary order of a hash map"))

(deftest adding-a-capability-appends-rather-than-reshuffling
  (testing "the tool list is part of the prompt a provider caches, so the tools
            already there must keep their positions"
    (let [before (tool-names many)
          after (tool-names (conj many {:id :extra
                                        :tools [{:name "tool_extra" :handler (fn [_] :x)}]}))]
      (is (= before (butlast after)))
      (is (= "tool_extra" (last after))))))

(deftest order-survives-however-the-tools-are-spread-across-capabilities
  (let [one-each (mapv (fn [n] {:id (keyword n) :tools [{:name n :handler (fn [_])}]})
                       ["a" "b" "c"])
        all-in-one [{:id :all :tools (mapv (fn [n] {:name n :handler (fn [_])}) ["a" "b" "c"])}]]
    (is (= ["a" "b" "c"] (tool-names one-each) (tool-names all-in-one)))))
