(ns jj.bettong.impl.edn-text-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.bettong.impl.edn-text :as sut]))

(def config
  (str ";; app config -- keep in sync with ops/nginx.conf\n"
       "{:server {:port 8080   ; the public port\n"
       "          :host \"localhost\"}\n"
       "\n"
       " ;; feature flags, one per line so diffs stay readable\n"
       " :flags [:beta         ; new search\n"
       "         :dark-mode]\n"
       "\n"
       " #_{:disabled {:legacy true}}\n"
       " :timeout #inst \"2026-01-01T00:00:00Z\"\n"
       " :debug false}\n"))

(defn- comments-of [s]
  (->> (str/split-lines s) (keep #(second (re-find #"(;;?.*)$" %))) vec))

(defn- blank-line-positions [s]
  (vec (keep-indexed (fn [i line] (when (str/blank? line) i)) (str/split-lines s))))

;; ================================================================= the tree

(deftest parse-and-render-are-inverses
  (testing "nothing is thrown away, so rendering returns the input byte for byte"
    (doseq [s [config
               "{}" "[]" "#{1 2}" "()"
               "{:a 1}"
               "   {:a 1}   \n\n"
               "{:a \"a } { ; string\" :b 2}"
               "{:a #_ignored 1}"
               "{:a #_{:whole :form} 1}"
               "{:chars [\\a \\newline \\; \\space]}"
               "{:t #inst \"2026-01-01T00:00:00Z\"}"
               "{:m ^:private {:c 1}}"
               "{:m ^{:doc \"x\"} [1]}"
               "{:q '(1 2) :d @x :u ~a :s ~@b}"
               ";; only a comment\n"
               "{:a 1} ;; trailing comment\n"]]
      (is (= s (sut/render (sut/parse s))) (str "round-tripping " (pr-str s))))))

(deftest the-tree-keeps-trivia-as-nodes
  (let [ast (sut/parse "{:a 1 ;; why\n :b 2}")
        types (map :type (:children (first (:children ast))))]
    (is (= :root (:type ast)))
    (is (= :map (:type (first (:children ast)))))
    (testing "whitespace and comments are nodes beside the values, not gaps"
      (is (some #{:comment} types))
      (is (some #{:whitespace} types))
      (is (some #{:token} types)))))

(deftest locate-finds-a-nodes-address
  (let [ast (sut/parse config)]
    (is (vector? (sut/locate ast [:server :port])))
    (is (= "8080" (:text (get-in ast (sut/locate ast [:server :port])))))
    (is (= ":dark-mode" (:text (get-in ast (sut/locate ast [:flags 1])))))
    (testing "and reports nothing for a path that is not there"
      (is (nil? (sut/locate ast [:nope])))
      (is (nil? (sut/locate ast [:server :nope])))
      (is (nil? (sut/locate ast [:flags 99]))))))

;; ========================================================= what must survive

(deftest an-edit-keeps-every-comment
  (let [updated (sut/set-in-str config [:server :port] 443)]
    (is (= (comments-of config) (comments-of updated)))
    (is (str/includes? updated ";; app config -- keep in sync with ops/nginx.conf"))
    (is (str/includes? updated ":port 443   ; the public port")
        "including the one trailing the value that changed")))

(deftest an-edit-changes-only-the-value
  (is (= (str/replace config "8080" "443") (sut/set-in-str config [:server :port] 443))))

(deftest an-edit-keeps-key-order
  (is (= [:server :flags :timeout :debug]
         (keys (edn/read-string (sut/set-in-str config [:debug] true))))))

(deftest discards-and-tagged-literals-are-untouched
  (let [updated (sut/set-in-str config [:debug] true)]
    (is (str/includes? updated "#_{:disabled {:legacy true}}"))
    (is (str/includes? updated "#inst \"2026-01-01T00:00:00Z\""))))

;; ============================================================== blank lines

(def spaced
  (str "{:a 1\n"
       "\n"
       "\n"
       "\n"
       " :b 2\n"
       "\n"
       "\n"
       " ;; a comment after the blanks\n"
       "\n"
       " :c 3}\n"
       "\n"))

(deftest runs-of-blank-lines-survive-an-edit
  (let [updated (sut/set-in-str spaced [:b] 99)]
    (is (= 99 (:b (edn/read-string updated))))
    (testing "every blank line is still exactly where it was"
      (is (= [1 2 3 5 6 8] (blank-line-positions spaced)) "three in a row, then two, then one")
      (is (= (blank-line-positions spaced) (blank-line-positions updated))))
    (testing "and the file is otherwise character for character the same"
      (is (= (str/replace spaced " :b 2" " :b 99") updated)))
    (testing "including the blank line at the end of the file"
      (is (str/ends-with? updated "}\n\n")))))

(deftest blank-lines-survive-an-insertion
  (let [updated (sut/set-in-str spaced [:d] 4)]
    (is (= {:a 1 :b 2 :c 3 :d 4} (edn/read-string updated)))
    (testing "the new entry goes below the last one, in its column"
      (is (str/includes? updated " :c 3\n :d 4}")))
    (testing "and no blank line moves"
      (is (= (blank-line-positions spaced) (blank-line-positions updated))))))

(deftest blank-lines-survive-a-deletion
  (let [updated (sut/delete-in-str spaced [:b])]
    (is (= {:a 1 :c 3} (edn/read-string updated)))
    (testing "the entry takes its own line and indentation with it, and no more"
      (is (= (str/replace spaced "\n\n\n\n :b 2" "") updated)))
    (testing "the comment below it stays"
      (is (str/includes? updated ";; a comment after the blanks")))
    (testing "the blank runs on either side merge rather than vanishing"
      (is (str/includes? updated "{:a 1\n\n\n ;; a comment")))))

(deftest blank-lines-inside-a-nested-collection-survive
  (let [src (str "{:server {:port 8080\n"
                 "\n"
                 "\n"
                 "          :host \"localhost\"}\n"
                 "\n"
                 " :flags [:a\n"
                 "\n"
                 "\n"
                 "         :b]}\n")
        updated (sut/set-in-str src [:server :host] "example.com")]
    (is (= "example.com" (get-in (edn/read-string updated) [:server :host])))
    (is (= (blank-line-positions src) (blank-line-positions updated)))
    (is (= (str/replace src "\"localhost\"" "\"example.com\"") updated))))

(deftest a-file-that-is-nothing-but-blank-lines-around-a-map
  (let [src "\n\n\n{:a 1}\n\n\n"
        updated (sut/set-in-str src [:a] 2)]
    (is (= "\n\n\n{:a 2}\n\n\n" updated))))

;; =================================================================== edits

(deftest sets-a-nested-value
  (is (= 443 (get-in (edn/read-string (sut/set-in-str config [:server :port] 443)) [:server :port]))))

(deftest sets-a-vector-element
  (let [updated (sut/set-in-str config [:flags 1] :light-mode)]
    (is (= [:beta :light-mode] (:flags (edn/read-string updated))))
    (is (str/includes? updated "; new search") "the comment inside the vector stays")))

(deftest sets-values-of-every-shape
  (doseq [v [443 "a string" :a-keyword true nil [1 2 3] {:nested {:deep true}} #{1 2}]]
    (is (= v (get-in (edn/read-string (sut/set-in-str config [:server :port] v)) [:server :port]))
        (str "for " (pr-str v)))))

(deftest adds-a-key-that-was-not-there
  (let [updated (sut/set-in-str config [:db] {:url "jdbc:pg"})]
    (is (= {:url "jdbc:pg"} (:db (edn/read-string updated))))
    (is (= (comments-of config) (comments-of updated)))
    (is (str/includes? updated "\n :db {:url \"jdbc:pg\"}}"))))

(deftest an-inline-map-stays-inline-when-a-key-is-added
  (is (= "{:a 1 :b 2}" (sut/set-in-str "{:a 1}" [:b] 2))))

(deftest creates-a-whole-missing-branch
  (let [updated (sut/set-in-str config [:db :pool :size] 10)]
    (is (= {:pool {:size 10}} (:db (edn/read-string updated))))
    (is (= (comments-of config) (comments-of updated)))))

(deftest deletes-a-key
  (let [updated (sut/delete-in-str config [:debug])]
    (is (not (contains? (edn/read-string updated) :debug)))
    (is (= (comments-of config) (comments-of updated)))
    (is (not (str/includes? updated ":debug")))))

(deftest deletes-a-nested-key
  (let [updated (sut/delete-in-str config [:server :host])]
    (is (= {:port 8080} (:server (edn/read-string updated))))
    (is (str/includes? updated "; the public port"))))

;; ============================================================ the safety net

(deftest an-edit-that-would-change-anything-else-is-refused
  (testing "the check re-reads the rendered text and compares it with the data intended"
    (with-redefs-fn {#'jj.bettong.impl.edn-text/node-of (fn [_] {:type :token :text "1 :sneaky 2"})}
      (fn []
        (is (thrown-with-msg? Exception #"would have changed more than the value asked for"
                              (sut/set-in-str config [:server :port] 443)))))))

(deftest an-edit-that-would-break-the-file-is-refused
  (with-redefs-fn {#'jj.bettong.impl.edn-text/node-of (fn [_] {:type :token :text "{{{"})}
    (fn []
      (is (thrown-with-msg? Exception #"unreadable EDN; file left alone"
                            (sut/set-in-str config [:server :port] 443))))))

(deftest an-empty-path-is-refused
  (is (thrown-with-msg? Exception #"path must name at least one key" (sut/set-in-str config [] 1)))
  (is (thrown-with-msg? Exception #"path must name at least one key" (sut/delete-in-str config []))))

(deftest deleting-a-missing-key-says-so
  (is (thrown-with-msg? Exception #"No such key: :nope" (sut/delete-in-str config [:nope]))))

(deftest a-keyword-key-into-a-vector-is-refused
  (testing "the data check rejects it before any text is produced"
    (is (thrown? Exception (sut/set-in-str config [:flags :x] 1)))))

;; ====================================================== the tricky syntax

(deftest structure-inside-strings-and-char-literals-is-not-structure
  (let [tricky "{:a \"} ; not a comment {\" :b \\; :c [1 2] :d 3}"]
    (is (= 9 (:d (edn/read-string (sut/set-in-str tricky [:d] 9)))))
    (is (= "} ; not a comment {" (:a (edn/read-string (sut/set-in-str tricky [:d] 9)))))
    (is (= [9 9] (:c (edn/read-string (sut/set-in-str tricky [:c] [9 9])))))))

(deftest discarded-forms-are-skipped-not-counted
  (let [s "{:a 1 #_:b #_{:c 2} :d 3}"]
    (is (= {:a 1 :d 9} (edn/read-string (sut/set-in-str s [:d] 9))))
    (is (str/includes? (sut/set-in-str s [:d] 9) "#_{:c 2}"))))

(deftest sets-and-metadata-are-navigable
  (let [s "{:a #{1 2 3} :b ^:private {:c 1}}"]
    (is (= #{4} (:a (edn/read-string (sut/set-in-str s [:a] #{4})))))
    (testing "metadata wraps the value, so the path goes through it"
      (is (= 2 (get-in (edn/read-string (sut/set-in-str s [:b :c] 2)) [:b :c])))
      (is (str/includes? (sut/set-in-str s [:b :c] 2) "^:private")))))
