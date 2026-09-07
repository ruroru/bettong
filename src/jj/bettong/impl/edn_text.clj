(ns jj.bettong.impl.edn-text
  "Editing EDN through a syntax tree that keeps everything a reader throws away.

   `clojure.edn/read-string` gives you data, and data has no comments, no key
   order and no indentation - so no writer can put them back. The tree here
   keeps whitespace, comments and `#_` discards as nodes of their own, which
   makes rendering it the exact original text and an edit a change to one node.

   Nodes are plain maps:

     {:type :whitespace :text \" \\n\\n\"}
     {:type :comment    :text \";; why this is 3000\"}
     {:type :token      :text \"8080\"}
     {:type :map        :open \"{\" :close \"}\" :children [...]}
     {:type :wrapper    :text \"#inst\" :children [...]}   ; #_ ^meta 'quote #tag

   `parse` and `render` are inverses: (= s (render (parse s))) for any EDN."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; ---------------------------------------------------------------- the parser

(def ^:private collections
  {\( {:type :list :open "(" :close ")"}
   \[ {:type :vector :open "[" :close "]"}
   \{ {:type :map :open "{" :close "}"}})

(def ^:private closing #{\) \] \}})

(defn- whitespace? [c]
  (or (Character/isWhitespace ^char c) (= \, c)))

(defn- delimiter? [c]
  (or (whitespace? c) (contains? collections c) (contains? closing c) (= \" c) (= \; c)))

(declare parse-node)

(defn- parse-children
  "Parses nodes until `close` (or the end of the string when nil)."
  [^String s i close]
  (let [n (count s)]
    (loop [i i children []]
      (cond
        (>= i n) (if close
                   (throw (ex-info "Unterminated collection in EDN" {:index i}))
                   [children i])
        (and close (== (int close) (int (.charAt s i)))) [children (inc i)]
        :else (let [[node next-i] (parse-node s i)]
                (recur next-i (conj children node)))))))

(defn- parse-whitespace [^String s i]
  (let [n (count s)
        end (loop [i i] (if (and (< i n) (whitespace? (.charAt s i))) (recur (inc i)) i))]
    [{:type :whitespace :text (subs s i end)} end]))

(defn- parse-comment [^String s i]
  (let [nl (.indexOf s "\n" (int i))
        end (if (neg? nl) (count s) nl)]
    [{:type :comment :text (subs s i end)} end]))

(defn- parse-string [^String s i]
  (let [n (count s)
        end (loop [j (inc i)]
              (cond
                (>= j n) (throw (ex-info "Unterminated string in EDN" {:index i}))
                (= \\ (.charAt s j)) (recur (+ j 2))
                (= \" (.charAt s j)) (inc j)
                :else (recur (inc j))))]
    [{:type :token :text (subs s i end)} end]))

(defn- parse-token [^String s i]
  (let [n (count s)
        end (loop [j i] (if (and (< j n) (not (delimiter? (.charAt s j)))) (recur (inc j)) j))]
    [{:type :token :text (subs s i end)} (max end (inc i))]))

(defn- parse-char-literal
  "\\a, \\newline, \\u0041 - the character right after the backslash is part of
   it however delimiting it looks."
  [^String s i]
  (let [n (count s)
        end (loop [j (+ i 2)] (if (and (< j n) (not (delimiter? (.charAt s j)))) (recur (inc j)) j))]
    [{:type :token :text (subs s i (min end n))} (min end n)]))

(defn- semantic?
  "Nodes that are values, as opposed to the whitespace and comments between
   them. A `#_` discard is trivia: it is text that is not data."
  [node]
  (and (contains? #{:token :map :vector :set :list :wrapper} (:type node))
       (not (:discard? node))))

(defn- parse-wrapper
  "A prefix that owns the form(s) after it: #_ ^meta 'quote #tag @deref.
   `forms` is how many values it takes - metadata takes two."
  [^String s i text forms discard?]
  (let [n (count s)]
    (loop [i (+ i (count text)) children [] seen 0]
      (if (or (>= i n) (= seen forms))
        [(cond-> {:type :wrapper :text text :children children}
           discard? (assoc :discard? true))
         i]
        (let [[node next-i] (parse-node s i)]
          (recur next-i (conj children node) (cond-> seen (semantic? node) inc)))))))

(defn- parse-dispatch [^String s i]
  (let [n (count s)
        c (when (< (inc i) n) (.charAt s (inc i)))]
    (cond
      (= \{ c) (let [[children next-i] (parse-children s (+ i 2) \})]
                 [{:type :set :open "#{" :close "}" :children children} next-i])
      (= \_ c) (parse-wrapper s i "#_" 1 true)
      (= \" c) (let [[node end] (parse-string s (inc i))]
                 [{:type :token :text (str "#" (:text node))} end])
      (= \( c) (let [[children next-i] (parse-children s (+ i 2) \))]
                 [{:type :list :open "#(" :close ")" :children children} next-i])
      ;; #inst "..." / #uuid "..." / #my/tag {...}
      :else (let [[tag _] (parse-token s (inc i))]
              (parse-wrapper s i (str "#" (:text tag)) 1 false)))))

(defn- parse-node [^String s i]
  (let [c (.charAt s i)]
    (cond
      (whitespace? c) (parse-whitespace s i)
      (= \; c) (parse-comment s i)
      (= \" c) (parse-string s i)
      (= \\ c) (parse-char-literal s i)
      (contains? collections c) (let [{:keys [type open close]} (collections c)
                                      [children next-i] (parse-children s (inc i) (first close))]
                                  [{:type type :open open :close close :children children} next-i])
      (= \# c) (parse-dispatch s i)
      (= \^ c) (parse-wrapper s i "^" 2 false)
      (= \~ c) (if (= \@ (get s (inc i))) (parse-wrapper s i "~@" 1 false) (parse-wrapper s i "~" 1 false))
      (contains? #{\' \` \@} c) (parse-wrapper s i (str c) 1 false)
      :else (parse-token s i))))

(defn parse
  "EDN text as a tree of nodes. Nothing is discarded, so `render` reproduces the
   input exactly."
  [^String s]
  (let [[children _] (parse-children s 0 nil)]
    {:type :root :children children}))

(defn render
  "The text a node stands for."
  [node]
  (case (:type node)
    :root (str/join (map render (:children node)))
    (:whitespace :comment :token) (:text node)
    :wrapper (str (:text node) (str/join (map render (:children node))))
    (str (:open node) (str/join (map render (:children node))) (:close node))))

;; ------------------------------------------------------------- navigation

(defn- semantic-indices [children]
  (vec (keep-indexed (fn [i node] (when (semantic? node) i)) children)))

(defn- value-of
  "Steps past a root or a wrapper to the value it holds, collecting the
   get-in keys taken to get there."
  [ks node]
  (case (:type node)
    (:root :wrapper) (if-let [i (last (semantic-indices (:children node)))]
                       ;; a wrapper's value is its last form: `^:private {}` is the map
                       (recur (conj ks :children i) (nth (:children node) i))
                       [ks node])
    [ks node]))

(defn- map-node? [node] (contains? #{:map} (:type node)))

(defn- child-index
  "Index in `:children` of the value under `k`: a map key's value, or the nth
   element of a vector."
  [node k]
  (let [children (:children node)
        indices (semantic-indices children)]
    (if (map-node? node)
      (some (fn [[key-i value-i]]
              (when (= k (edn/read-string (render (nth children key-i))))
                value-i))
            (partition 2 indices))
      (when (and (integer? k) (< -1 k (count indices)))
        (nth indices k)))))

(defn locate
  "The `get-in` keys that reach the node holding the value at `path`, or nil."
  [ast path]
  (loop [ks [] node ast p (seq path)]
    (let [[ks node] (value-of ks node)]
      (if (nil? p)
        ks
        (when-let [i (child-index node (first p))]
          (recur (conj ks :children i) (nth (:children node) i) (next p)))))))

(defn- node-of
  "A Clojure value as a node, by printing it and parsing that back - so an
   inserted map is a real subtree, not an opaque blob of text."
  [v]
  (let [ast (parse (pr-str v))
        i (first (semantic-indices (:children ast)))]
    (nth (:children ast) i)))

;; ---------------------------------------------------------------- editing

(defn- indentation-before
  "The whitespace the last entry of a map sits behind, so an inserted key lands
   in the same column. An inline map stays inline."
  [children]
  (let [indices (semantic-indices children)
        last-key (first (last (partition 2 indices)))
        before (when (and last-key (pos? last-key)) (nth children (dec last-key)))]
    (if (and before (= :whitespace (:type before)) (str/includes? (:text before) "\n"))
      (str "\n" (last (str/split (:text before) #"\n" -1)))
      " ")))

(defn- last-significant
  "The last node that is not whitespace - what a new entry would sit behind."
  [children]
  (last (remove #(= :whitespace (:type %)) children)))

(defn- assoc-child
  "The map node with `k` added at the end, laid out like the entries above it."
  [node k v]
  (when-not (map-node? node)
    (throw (ex-info (str "Cannot add the key " (pr-str k) " to a " (name (:type node)))
                    {:key k :type (:type node)})))
  (update node :children
          (fn [children]
            (let [indent (indentation-before children)
                  ;; a `;` comment runs to the end of its line, so an entry put
                  ;; after one has to start on the next line or be eaten by it
                  indent (if (and (= :comment (:type (last-significant children)))
                                  (not (str/includes? indent "\n")))
                           (str "\n" indent)
                           indent)]
              (into (vec children)
                    [{:type :whitespace :text indent}
                     (node-of k)
                     {:type :whitespace :text " "}
                     (node-of v)])))))

(defn- dissoc-child
  "The map node with `k`, its value, and the whitespace that indented it
   removed. Comments before the key stay: they are not part of the entry."
  [node k]
  (let [children (vec (:children node))
        indices (semantic-indices children)
        [key-i value-i] (or (some (fn [[key-i value-i]]
                                    (when (= k (edn/read-string (render (nth children key-i))))
                                      [key-i value-i]))
                                  (partition 2 indices))
                            (throw (ex-info (str "No such key: " (pr-str k)) {:key k})))
        from (if (and (pos? key-i) (= :whitespace (:type (nth children (dec key-i)))))
               (dec key-i)
               key-i)
        ;; taking the indentation away from behind a comment would leave whatever
        ;; follows on the comment's line, closing brace included
        after-comment? (and (pos? from) (= :comment (:type (nth children (dec from)))))]
    (assoc node :children
           (-> (subvec children 0 from)
               (cond-> after-comment? (conj {:type :whitespace :text "\n"}))
               (into (subvec children (inc value-i)))))))

(defn- deepest-existing
  "How much of `path` the tree already has, as [keys-found location]. The rest
   has to be created."
  [ast path]
  (some (fn [k] (when-let [ks (locate ast (subvec path 0 k))] [k ks]))
        (range (dec (count path)) -1 -1)))

(defn- verify!
  "Re-reads the edited text and refuses it unless the data is exactly what was
   asked for. Text surgery you cannot check is text surgery you cannot trust."
  [updated expected path]
  (let [actual (try
                 (edn/read-string updated)
                 (catch Exception e
                   (throw (ex-info "Edit would have produced unreadable EDN; file left alone"
                                   {:path path} e))))]
    (when-not (= expected actual)
      (throw (ex-info "Edit would have changed more than the value asked for; file left alone"
                      {:path path :expected expected :actual actual})))
    updated))

(defn set-in-str
  "EDN text with `path` set to `value`, as an edit to one node of the tree.
   Every comment, blank line and column outside that node is the text that was
   already there."
  [^String s path value]
  (when (empty? path)
    (throw (ex-info "path must name at least one key" {})))
  (let [path (vec path)
        ast (parse s)
        expected (assoc-in (edn/read-string s) path value)
        updated (if-let [ks (locate ast path)]
                  (assoc-in ast ks (node-of value))
                  (let [[k ks] (or (deepest-existing ast path)
                                   (throw (ex-info (str "No collection at "
                                                        (pr-str (vec (butlast path))))
                                                   {:path path})))
                        ;; keys below what exists become nested maps in one edit
                        nested (reduce (fn [acc key] {key acc}) value (reverse (subvec path (inc k))))
                        [ks node] (value-of ks (get-in ast ks))]
                    (assoc-in ast ks (assoc-child node (nth path k) nested))))]
    (verify! (render updated) expected path)))

(defn delete-in-str
  "EDN text with `path` removed, leaving the formatting around it alone."
  [^String s path]
  (when (empty? path)
    (throw (ex-info "path must name at least one key" {})))
  (let [path (vec path)
        ast (parse s)
        before (edn/read-string s)
        expected (if (= 1 (count path))
                   (dissoc before (first path))
                   (update-in before (butlast path) dissoc (last path)))
        ks (or (locate ast (butlast path))
               (throw (ex-info (str "No such key: " (pr-str (last path))) {:path path})))
        [ks node] (value-of ks (get-in ast ks))
        updated (assoc-in ast ks (dissoc-child node (last path)))]
    (verify! (render updated) expected path)))
