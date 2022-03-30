(use 'clojure.string)
(require '[clojure.set :as s])

(def data (->> (split-lines (slurp "data/input_5")) (map seq)))

(def rows (range 128))
(def columns (range 8))

(defn halves [coll]
  (split-at (-> (count coll) (/ 2)) coll))

(defn b-search [up down coll path]
  (loop [rest-coll coll
         rest-path path]
    (if (empty? rest-path)
      (first rest-coll)
      (let [[lower upper] (halves rest-coll)
            [head & tail] rest-path]
        (recur ({up upper down lower} head) tail)))))

(def find-row (partial b-search \B \F rows))
(def find-col (partial b-search \R \L columns))

(defn seat-id [[row column]]
  (+ column (* 8 row)))

(defn seat [path]
  (let [[row-path column-path] (split-at 7 path)]
    [(find-row row-path) (find-col column-path)]))

(def middle-seats
  (for [r (butlast (rest rows))
        c columns]
    [r c]))

(def taken-seats
  (sort
    (s/intersection
      (set middle-seats)
      (set (->> data (map seat))))))

(def taken-seat-ids
  (dedupe
    (sort (map seat-id taken-seats))))

(->> (partition 2 1 taken-seat-ids) (filter (comp (partial not= -1) #(apply - %))))
