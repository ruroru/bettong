(ns jj.bettong.impl.shell
  "Running commands behind the shell capability: the command policy, the
   process itself, and the truncation that keeps its output from swallowing
   the transcript."
  (:require [clojure.string :as str]
            [jj.bettong.impl.files :as files]
            [jj.bettong.secrets :as secrets])
  (:import [java.io File]
           [java.util List]
           [java.util.concurrent TimeUnit]))

(def default-shell-timeout-ms 120000)
(def default-max-output 20000)

(defn truncate
  "Keeps the head and tail of `s`, since the interesting part of command output
   is usually at one end or the other."
  [s limit]
  (if (<= (count s) limit)
    s
    (let [head (quot limit 4)
          tail (- limit head)]
      (str (subs s 0 head)
           (format "\n... [%d characters truncated] ...\n" (- (count s) limit))
           (subs s (- (count s) tail))))))

(def denied-commands
  "Commands refused by default. This is a seatbelt, not a sandbox: it stops the
   catastrophic mistakes an agent actually makes, and a determined command can
   still work around any pattern. If you need a real boundary, use an allowlist
   or do not grant the shell."
  [[#"(?i)\brm\b[^|;&]*\s+-[a-z]*[rR][a-z]*\s+/(\s|$|\*)" "recursive delete of /"]
   [#"(?i)\bmkfs(\.|\s)" "formatting a filesystem"]
   [#"(?i)\bdd\b[^|;&]*\bof=/dev/" "writing straight to a device"]
   [#"(?i)\b(shutdown|reboot|poweroff|halt)\b" "shutting the machine down"]
   [#":\(\)\s*\{[^}]*\|[^}]*&[^}]*\}\s*;?\s*:" "a fork bomb"]
   [#"(?i)\b(curl|wget)\b[^|;&]*\|\s*(sudo\s+)?[a-z]*sh\b" "piping a download into a shell"]
   [#"(?i)\bchmod\b[^|;&]*\s777\s+/(\s|$)" "making / world-writable"]])

(defn- check-command!
  [command {:keys [deny deny-defaults? allow] :or {deny-defaults? true}}]
  (when (and allow (not (some #(re-find % command) allow)))
    (throw (ex-info (str "refused: this shell only runs commands matching its allowlist")
                    {:command command})))
  (doseq [[pattern reason] (cond-> (vec deny)
                             deny-defaults? (into denied-commands))]
    (let [[pattern reason] (if (vector? pattern) pattern [pattern reason])]
      (when (re-find pattern command)
        (throw (ex-info (str "refused: " (or reason "denied by policy")) {:command command}))))))

(defn run-shell
  "Runs `command` through bash and returns its exit code plus combined
   stdout/stderr. Never throws on a non-zero exit - a failing command is a
   result the model should read, not an error."
  [{:keys [shell-timeout-ms max-output strip-secrets?] :or {strip-secrets? true} :as opts}
   {:keys [command dir]}]
  (when (str/blank? (str command))
    (throw (ex-info "command must not be blank" {})))
  (check-command! (str command) opts)
  (let [^File work (files/resolve-path (assoc opts :allow-secrets? true) (or dir "."))
        _ (when-not (.isDirectory work)
            (throw (ex-info (str "No such directory: " work) {:path (str work)})))
        timeout-ms (or shell-timeout-ms default-shell-timeout-ms)
        limit (or max-output default-max-output)
        ;; ProcessBuilder takes either a List or a String... - the compiler needs
        ;; a hinted local to tell which, a hint on the literal is not enough
        ^List argv ["bash" "-c" (str command)]
        builder (doto (ProcessBuilder. argv)
                  (.directory work)
                  (.redirectErrorStream true))
        _ (when strip-secrets? (secrets/strip-env! builder))
        ^Process proc (.start builder)
        _ (.close (.getOutputStream proc))          ; no stdin: don't let it hang on a prompt
        output (future (slurp (.getInputStream proc)))
        finished? (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
    (if finished?
      (let [text (truncate (str/trim @output) limit)]
        (format "exit %d%s" (.exitValue proc)
                (if (str/blank? text) "\n(no output)" (str "\n" text))))
      (do (.destroyForcibly proc)
          (.waitFor proc 5 TimeUnit/SECONDS)
          (format "ERROR: command timed out after %dms and was killed\n%s"
                  timeout-ms
                  (truncate (str/trim (or (deref output 1000 "") "")) limit))))))
