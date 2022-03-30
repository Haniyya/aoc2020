(ns aoc2021.03
  (:require [clojure.string :as s]))

(def data
  (->> (slurp "2021/data/03")
       (s/split-lines)
       (map seq)))

(defn bit-frequencies [numbers]
  (->> numbers
    (apply map list)
    (map frequencies)))

(defn solution1 []
  (->> [max-key min-key]
    (map (fn [sortfn]
          (->> (bit-frequencies data)
            (map #(apply sortfn last %))
            (map first)
            s/join)))
    (map #(Integer/parseInt % 2))
    (reduce *)))

(defn filter-numbers [idx sortfn numbers]
  (let [sign-chars (->> numbers bit-frequencies (map #(apply sortfn last %)) (map first))
        sign-filter (fn [line] (= (nth line idx) (nth sign-chars idx)))]
    (filter sign-filter numbers)))

(defn min-key* [k & pairs]
  (let [results (->> pairs (map k))]
    (if (every? #(= (first results) %) results)
      [\0 (first results)]
      (apply min-key k pairs))))

(defn max-key* [k & pairs]
  (let [results (->> pairs (map k))]
    (if (every? #(= (first results) %) results)
      [\1 (first results)]
      (apply max-key k pairs))))

(defn filter-walk
  ([sortfn numbers] (filter-walk sortfn numbers 0))
  ([sortfn numbers idx]
   (if (-> numbers count (= 1))
      (first numbers)
      (recur sortfn (filter-numbers idx sortfn numbers) (inc idx)))))

(defn solution2 []
  (->> [max-key* min-key*]
      (map #(filter-walk % data))
      (map s/join)
      (map #(Integer/parseInt % 2))
      (reduce *)))
