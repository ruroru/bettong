(ns jj.bettong.test-server
  "A real HTTP server for the tests, on ring-http-exchange (the JDK's
   com.sun.net.httpserver). Backends and the LLM client take a :base-url, so
   pointing them here exercises the whole request path - query params, headers,
   status handling - without touching the network."
  (:require [clojure.string :as str]
            [ring-http-exchange.core :as server])
  (:import [java.net ServerSocket]))

(defn free-port
  "An unused port. The server needs a concrete one - it rejects port 0."
  []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn query-params
  "Parses a ring :query-string into a map of string -> string."
  [query-string]
  (into {}
        (for [pair (str/split (or query-string "") #"&")
              :when (seq pair)
              :let [[k v] (str/split pair #"=" 2)]]
          [(java.net.URLDecoder/decode k "UTF-8")
           (java.net.URLDecoder/decode (or v "") "UTF-8")])))

(defn- lowercase-headers
  "com.sun.net.httpserver hands headers back as 'X-api-key' / 'User-agent'.
   Lowercase them so tests can assert in the usual ring style."
  [headers]
  (into {} (for [[k v] headers] [(str/lower-case (name k)) v])))

(defn start
  "Starts a server answering with `respond`, which is either a ring response map
   or a function of the recorded request. Returns
   {:base-url .. :requests <atom of recorded requests> :stop <fn>}."
  [respond]
  (let [port (free-port)
        requests (atom [])
        handler (fn [request]
                  (let [recorded (-> request
                                     (select-keys [:request-method :uri :query-string])
                                     (assoc :headers (lowercase-headers (:headers request)))
                                     (assoc :body (when-let [body (:body request)] (slurp body)))
                                     (assoc :params (query-params (:query-string request))))]
                    (swap! requests conj recorded)
                    (let [response (if (fn? respond) (respond recorded) respond)]
                      (merge {:status 200 :headers {"Content-Type" "application/json"}} response))))
        instance (server/run-http-server handler {:port port :host "127.0.0.1"})]
    {:base-url (str "http://127.0.0.1:" port)
     :requests requests
     :stop #(server/stop-http-server instance)}))

(defmacro with-server
  "(with-server [s (fn [req] {:status 200 :body \"...\"})] ...) - stops the
   server afterwards whatever happens."
  [[sym respond] & body]
  `(let [~sym (start ~respond)]
     (try ~@body
          (finally ((:stop ~sym))))))

(defn requests [server] @(:requests server))

(defn only-request [server]
  (let [rs (requests server)]
    (assert (= 1 (count rs)) (str "expected exactly one request, got " (count rs)))
    (first rs)))

(defn json-response [body]
  {:status 200 :headers {"Content-Type" "application/json"} :body body})
