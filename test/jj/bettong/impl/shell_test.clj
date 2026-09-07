(ns jj.bettong.impl.shell-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.impl.shell :as shell]))

(defn tmp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "bettong-test" (into-array java.nio.file.attribute.FileAttribute []))))

(deftest shell-returns-exit-code-and-output
  (is (= "exit 0\nhello" (shell/run-shell {} {:command "echo hello"}))))

(deftest shell-merges-stderr-into-output
  (is (= "exit 0\nto stderr" (shell/run-shell {} {:command "echo 'to stderr' 1>&2"}))))

(deftest shell-reports-failure-instead-of-throwing
  (let [result (shell/run-shell {} {:command "exit 3"})]
    (is (str/starts-with? result "exit 3"))
    (is (str/includes? result "(no output)"))))

(deftest shell-keeps-going-after-a-failing-command
  (let [result (shell/run-shell {} {:command "ls /definitely/not/here"})]
    (is (not (str/starts-with? result "exit 0")))
    (is (str/includes? result "No such file"))))

(deftest shell-runs-in-the-requested-directory
  (let [dir (tmp-dir)]
    (spit (str dir "/marker.txt") "")
    (is (str/includes? (shell/run-shell {} {:command "ls" :dir dir}) "marker.txt"))
    (is (str/includes? (shell/run-shell {} {:command "pwd" :dir dir}) dir))))

(deftest shell-dir-respects-the-root
  (let [dir (tmp-dir)]
    (is (str/includes? (shell/run-shell {:root dir} {:command "pwd" :dir "."}) dir))
    (is (thrown-with-msg? Exception #"escapes the allowed root"
                          (shell/run-shell {:root dir} {:command "pwd" :dir "/etc"})))))

(deftest shell-rejects-a-blank-command
  (is (thrown-with-msg? Exception #"must not be blank" (shell/run-shell {} {:command "  "})))
  (is (thrown-with-msg? Exception #"must not be blank" (shell/run-shell {} {}))))

(deftest shell-rejects-a-missing-directory
  (is (thrown-with-msg? Exception #"No such directory"
                        (shell/run-shell {} {:command "pwd" :dir (str (tmp-dir) "/nope")}))))

(deftest shell-kills-a-command-that-overruns
  (let [start (System/currentTimeMillis)
        result (shell/run-shell {:shell-timeout-ms 400} {:command "sleep 30"})
        elapsed (- (System/currentTimeMillis) start)]
    (is (str/includes? result "timed out after 400ms"))
    (is (< elapsed 15000) "it should not have waited for the sleep to finish")))

(deftest shell-does-not-hang-on-a-command-that-reads-stdin
  (let [result (shell/run-shell {:shell-timeout-ms 5000} {:command "cat"})]
    (is (str/starts-with? result "exit 0") "stdin is closed, so cat sees EOF immediately")))

(deftest shell-truncates-huge-output
  (let [result (shell/run-shell {:max-output 500} {:command "seq 1 20000"})]
    (is (< (count result) 700))
    (is (str/includes? result "characters truncated"))
    (testing "both ends survive"
      (is (str/includes? result "1\n2\n3"))
      (is (str/includes? result "20000")))))

(deftest truncate-leaves-short-strings-alone
  (is (= "short" (shell/truncate "short" 100)))
  (is (= "exactly10!" (shell/truncate "exactly10!" 10))))

(deftest truncate-keeps-head-and-tail
  (let [result (shell/truncate (str/join (repeat 100 "abcdefghij")) 100)]
    (is (str/starts-with? result "abcdefghijabcdefghijabcde"))
    (is (str/ends-with? result "abcdefghij"))
    (is (str/includes? result "[900 characters truncated]"))))

