(ns jj.bettong.impl.files-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.impl.files :as files]))

(defn tmp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "bettong-test" (into-array java.nio.file.attribute.FileAttribute []))))

(deftest write-file-creates-file
  (let [dir (tmp-dir)
        path (str dir "/foo.txt")
        result (files/write-file {} {:path path :content "foo"})]
    (is (.isFile (io/file path)))
    (is (= "foo" (slurp path)))
    (is (str/includes? result "Wrote 3 bytes"))))

(deftest write-file-creates-parent-dirs
  (let [dir (tmp-dir)
        path (str dir "/a/b/c/deep.txt")]
    (files/write-file {} {:path path :content "hi"})
    (is (= "hi" (slurp path)))))

(deftest write-file-overwrites
  (let [dir (tmp-dir)
        path (str dir "/x.txt")]
    (files/write-file {} {:path path :content "one"})
    (files/write-file {} {:path path :content "two"})
    (is (= "two" (slurp path)))))

(deftest write-file-handles-missing-content
  (let [dir (tmp-dir)
        path (str dir "/empty.txt")]
    (files/write-file {} {:path path})
    (is (= "" (slurp path)))))

(deftest write-file-counts-utf8-bytes
  (let [dir (tmp-dir)
        path (str dir "/u.txt")]
    ;; "é" is two bytes in UTF-8, one character in Java
    (is (str/includes? (files/write-file {} {:path path :content "é"}) "2 bytes"))))

(deftest read-file-round-trips
  (let [dir (tmp-dir)
        path (str dir "/r.txt")]
    (spit path "contents here")
    (is (= "contents here" (files/read-file {} {:path path})))))

(deftest read-file-throws-on-missing
  (is (thrown-with-msg? Exception #"No such file"
                        (files/read-file {} {:path (str (tmp-dir) "/nope.txt")}))))

(deftest list-dir-lists-entries
  (let [dir (tmp-dir)]
    (spit (str dir "/b.txt") "")
    (.mkdirs (io/file dir "a-dir"))
    (is (= "a-dir/\nb.txt" (files/list-dir {} {:path dir})))))

(deftest list-dir-reports-empty
  (is (= "(empty directory)" (files/list-dir {} {:path (tmp-dir)}))))

(deftest delete-file-removes-file
  (let [dir (tmp-dir)
        path (str dir "/gone.txt")]
    (spit path "x")
    (files/delete-file {} {:path path})
    (is (not (.exists (io/file path))))))

(deftest blank-path-rejected
  (is (thrown-with-msg? Exception #"blank" (files/write-file {} {:path "" :content "x"})))
  (is (thrown-with-msg? Exception #"blank" (files/write-file {} {:content "x"}))))

(deftest root-confines-relative-paths
  (let [dir (tmp-dir)]
    (files/write-file {:root dir} {:path "inside.txt" :content "yes"})
    (is (= "yes" (slurp (str dir "/inside.txt"))))))

(deftest root-blocks-traversal
  (let [dir (tmp-dir)]
    (is (thrown-with-msg? Exception #"escapes the allowed root"
                          (files/write-file {:root dir} {:path "../escaped.txt" :content "no"})))
    (is (thrown-with-msg? Exception #"escapes the allowed root"
                          (files/write-file {:root dir} {:path "/etc/passwd" :content "no"})))
    (is (not (.exists (io/file dir ".." "escaped.txt"))))))

(deftest no-root-means-anywhere
  (let [path (str (tmp-dir) "/../free.txt")]
    (files/write-file {} {:path path :content "ok"})
    (is (= "ok" (slurp path)))
    (io/delete-file path)))

