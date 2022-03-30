(require '[clojure.core.logic.fd :as fd])
(use 'clojure.string)
(use 'clojure.core.logic)

(def data
  (->> (slurp "data/input_10") split-lines (map #(Integer/parseInt %))))

(def full-list
  (let [sorted-data (sort data)]
    (concat '(0) sorted-data (list (+ 3 (last sorted-data))))))

(def solution1
  (->> full-list
      (partition 2 1)
      (map (partial apply -))
      frequencies
      vals
      (apply *)))

; Part 2

(defn reachable-tails [n l]
  (let [tails (take-while seq (iterate rest l))]
    (take-while (comp #(<= 1 % 3) #(- % n) first) tails)))

(defn count-paths [[head & tail]]
  (if (nil? tail)
    1
    (->> (reachable-tails head tail) (map count-paths) (reduce +))))

(def solution2
  (->> full-list
    (partition 2 1) ; All jumps
    (partition-by #(apply - %)) ; Devide into chains of small and big jumps
    (map (comp dedupe flatten)) ; Merge pairs into chains
    (map count-paths) ; Only count paths inside sub-graph
    (reduce *)))
