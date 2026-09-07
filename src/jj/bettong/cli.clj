(ns jj.bettong.cli
  "CLI entry point. Grants a writable filesystem and a shell - the explicit
   opt-in a program has to make, since reading is all a capability gives by default:
     lein run \"Create file /tmp/foo.txt with content 'foo'\""
  (:require [clojure.string :as str]
            [jj.bettong.agent :as agent]
            [jj.bettong.capabilities :as caps]
            [jj.bettong.usage :as usage])
  (:gen-class))

(defn -main [& args]
  (let [prompt (str/join " " args)]
    (when (str/blank? prompt)
      (println "usage: lein run \"<task>\"")
      (System/exit 1))
    (let [assistant (agent/deepseek
                     {:capabilities [(caps/filesystem {:writable? true}) (caps/shell)]
                      :on-event (fn [{:keys [event tool args]}]
                                  (when (= event :tool-call)
                                    (println (str "  · " tool " " (pr-str args)))))})
          {:keys [output done?] :as result} (agent/run assistant prompt)]
      (println (or output "(no final answer - hit the step limit)"))
      (println (str "  " (usage/summary (:usage result))))
      (System/exit (if done? 0 1)))))
