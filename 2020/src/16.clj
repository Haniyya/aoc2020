(use 'clojure.string)
(use 'clojure.core.logic)
(require '[clojure.set :as s])

;; Data preparation

(defn parse-interval [interval-str]
  (let [[_ lower upper] (re-find #"(\d+)-(\d+)" interval-str)]
    (->> [lower upper] (mapv #(Integer/parseInt %)))))

(defn parse-field-line [field-str]
  (let [[_ left-int right-int] (re-find #".+: (.+) or (.+)" field-str)]
    (mapv parse-interval [left-int right-int])))

(defn parse-intervals [field-block]
  (->> field-block split-lines (map parse-field-line) (apply concat)))

(defn parse-tickets [ticket-block]
  (let [without-header (drop 1 (split-lines ticket-block))]
    (->> without-header
        (mapv #(split % #","))
        (mapv (fn [numbers] (mapv (comp #(Integer/parseInt %) trim) numbers))))))

(defn parse-input [input]
  (let [[intervals my-ticket tickets] (split input #"\n\n")]
    [(parse-intervals intervals) my-ticket (parse-tickets tickets)]))

(defn parse-attribute-line [attribute-line]
  (let [[_ attribute] (re-find #"(.+):" attribute-line)
        intervals (parse-intervals attribute-line)]
    (mapv vector intervals (repeat attribute))))

(defn parse-field-map [field-block]
  (->> field-block split-lines (mapcat parse-attribute-line) (into {})))

(def sample-intervals (parse-intervals "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50"))

(def sample-map (parse-field-map "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50"))

(def sample-tickets (parse-tickets "nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"))

(def data (parse-input (slurp "data/input_16")))
(def tickets (last data))
(def intervals (first data))
(def field-map (parse-field-map (first (split (slurp "data/input_16") #"\n\n"))))

(defn augment-node [interval]
  {:interval interval :annotation (last interval) :left nil :right nil})

(def lower (comp first :interval))

(defn leaf? [node]
  (every? nil? ((juxt :left :right) node)))

(defn insert [tree node]
  (if (nil? tree)
    node
    (let [tlower (lower tree)
          nlower (lower node)
          annotation (max (:annotation tree) (:annotation node))]
      (cond
        (<= nlower tlower) (assoc tree :left (insert (:left tree) node))
        (> nlower tlower) (merge tree {:annotation annotation :right (insert (:right tree) node)})))))

(defn member? [tree value]
  (let [[tlower tupper] (:interval tree)]
    (cond
      (<= tlower value tupper) true
      (> value (:annotation tree)) false
      (> value tupper) (and (some? (:right tree)) (member? (:right tree) value))
      (< value tlower) (and (some? (:left tree)) (member? (:left tree) value)))))

(defn matching [tree value]
    (if (nil? tree)
      #{}
      (let [[tlower tupper] (:interval tree)
            [left right] ((juxt :left :right) tree)
            current-matching (if (<= tlower value tupper) #{[tlower tupper]} #{})]
        (cond
          (> value (:annotation tree)) current-matching
          :else (s/union current-matching (matching left value) (matching right value))))))

(defn possible-fields [field-map intervals]
  (->> intervals (select-keys field-map) vals set))

(defn augmented-tree [intervals]
  (->> intervals (map augment-node) (reduce insert)))

(defn valid? [tree ticket]
  (every? (partial member? tree) ticket))

(defn solve []
  (let [tree (augmented-tree intervals)]
    (->> tickets
         (filter (partial valid? tree))
         (map #(mapv (partial matching2 intervals) %))
         (map #(mapv (partial possible-fields field-map) %))
         (reduce (fn [fst snd] (mapv s/intersection fst snd))))))

(defn remove-from-all [solution space]
  (->> space
    (map (fn [[idx elem]] [idx (s/difference elem solution)]))
    (filter (comp not-empty last))))

(defn determine [solution-space]
  (loop [undetermined (map-indexed vector solution-space)
         determined (vec (repeat (count solution-space) nil))]
    (if (empty? undetermined)
      determined
      (let [[idx solution] (first (filter (comp #(= 1 %) count last) undetermined))]
        (recur (remove-from-all solution undetermined) (assoc determined idx (first solution)))))))

(defn matching2 [intervals value]
  (filter (fn [[lower upper]] (<= lower value upper)) intervals))

(comment
  (let [my-ticket (first (parse-tickets (second data)))]
    (->> my-ticket
      (map vector (determine (solve)))
      (filter (comp #(re-matches #"departure.*" %) first))
      (map last)
      (apply *'))))

(comment
  (let [tree (augmented-tree intervals)
        field-map (parse-field-map (first (split (slurp "data/input_16") #"\n\n")))]
    (->> 100 (matching tree) (possible-fields field-map))))
