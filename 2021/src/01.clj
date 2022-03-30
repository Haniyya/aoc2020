(ns aoc2020.01
  (:require [clojure.string :as s]))

(def data
  (->> (slurp "2021/data/01")
   (s/split-lines)
   (map #(Integer/parseInt %))))

(defn solution1 []
  (->> data
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(defn solution2 []
  (letfn [(sum [l] (map #(apply + %) l))
          (cmp [pair] (apply < pair))]
    (->> data
        (partition 3 1)
        (partition 2 1)
        (map sum)
        (filter cmp)
        count)))
