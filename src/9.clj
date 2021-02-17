(require '[clojure.core.logic.fd :as fd])
(use 'clojure.core.logic)
(use 'clojure.string)

(def data
  (->> (slurp "data/input_9") split-lines (map #(BigInteger. %))))

(defn summands [n preamble]
  (run 1 [q]
    (fresh [x y]
      (membero x preamble)
      (membero y preamble)
      (!= x y)
      (fd/+ x y n)
      (== q [x y]))))

(defn invalid? [nchunk]
  (let [[preamble n] (split-at 25 nchunk)]
    (empty? (summands (first n) preamble))))

(def solution1
  (->> data (partition 26 1) (filter invalid?) first last))

;; Part 2

(defn sumo2 [l sum]
  (conde
    [(== sum 0)]
    [(fd/< 0 sum)
     (fresh [head tail rsum]
       (conso head tail l)
       (fd/+ rsum head sum)
       (sumo2 tail rsum))]))

(defn prefix-sumo [sum l q]
  (fresh [t]
    (appendo q t l)
    (sumo2 q sum)))

(defn sublist-sumo [sum l q]
  (conde
    [(prefix-sumo sum l q)]
    [(fresh [head tail]
        (conso head tail l)
        (sublist-sumo sum tail q))]))

(comment
  "Pretty sure this works but takes forever to compute"
  (run 1 [q]
    (sublist-sumo solution1 data q)))

(defn prefix-sum [sum l]
  (loop [numbers []
         rsum sum
         [head & tail] l]
    (cond
      (= rsum 0) numbers
      (or (< rsum 0) (empty? tail)) nil
      :else (recur (conj numbers head) (- rsum head) tail))))

(defn sublist-sum [sum l]
  (let [enum (range (count l))]
    (->> enum (map #(drop % l)) (keep (partial prefix-sum sum)) first)))

(def solution2
  (->> (sublist-sum solution1 data) ((juxt (partial apply min) (partial apply max))) (apply +)))
