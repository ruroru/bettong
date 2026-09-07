(ns jj.bettong.capability.shell
  "Running shell commands."
  (:require [jj.bettong.impl.shell :as shell]))

(defn shell
  "Run shell commands. Options:
     :root       working directory the command's :dir is resolved against and
                 confined to. NOTE this confines where the command starts, not
                 what it can reach - a command can write anywhere the process can.
     :timeout-ms kill a command after this long (default 120000)
     :max-output characters of output to keep (default 20000)
     :strip-secrets?  keep credential env vars from the command (default true)
     :deny            extra [regex reason] pairs to refuse
     :deny-defaults?  refuse the catastrophic commands (default true)
     :allow           regexes; when given, only matching commands run"
  ([] (shell {}))
  ([{:keys [timeout-ms] :as options}]
   ;; forward everything: the policy options belong to run-shell, and a dropped
   ;; :deny or :allow would look like it applied while doing nothing
   (let [opts (-> options (dissoc :timeout-ms) (assoc :shell-timeout-ms timeout-ms))]
     {:id :shell
      :instructions (str "You can run shell commands with run_shell. It is non-interactive - stdin is "
                         "closed - so never run a command that waits for input.")
      :tools [{:name "run_shell"
               :description (str "Run a shell command with bash and return its exit code and combined "
                                 "stdout/stderr. Use it for what the other tools cannot do: running tests, "
                                 "git, package managers, searching.")
               :parameters {:command {:type "string" :description "The shell command to run." :required true}
                            :dir {:type "string" :description "Working directory. Defaults to the current directory."}}
               :handler #(shell/run-shell opts %)}]})))
