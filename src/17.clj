(require '[clojure.set :as s])
(use 'clojure.core.logic)
(use 'clojure.string)

(defn coordinates [[idx characters]]
  (map-indexed #(vector idx %1 %2) characters))

(def data
  (->> (slurp "data/input_17")
       split-lines
       (map-indexed vector)
       (mapcat coordinates)
       (filter (comp #(= \# %) last))
       (map #(assoc % 2 0))
       (into #{})))

(def data2
  (into #{} (map #(cons 0 %)) data))

(defn idx-neighbor [n]
  [(dec n) n (inc n)])

(def neighbors
  (memoize
    (fn [pos]
      (let [lvars (repeatedly (count pos) lvar)
            ranges (mapv idx-neighbor pos)]
        (run* [neighbor]
          (and* (mapv membero lvars ranges))
          (!= neighbor pos)
          (== neighbor lvars))))))

(defn activate? [actives pos]
  (let [active-neighbors (filter actives (neighbors pos))]
    (= 3 (bounded-count 4 active-neighbors))))

(defn deactivate? [actives pos]
  (let [active-neighbors (filter actives (neighbors pos))]
    ((complement #{2 3}) (bounded-count 4 active-neighbors))))

(defn next-actives [actives]
  (let [all-neighbors (into #{} (mapcat neighbors) actives)
        inactives (s/difference all-neighbors actives)
        activated (filter #(activate? actives %) inactives)
        deactivated (filter #(deactivate? actives %) actives)]
    (s/union (set activated) (s/difference actives deactivated))))

(comment
  (time
    (count (nth (iterate next-actives data2) 6))))
