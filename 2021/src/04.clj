(ns aoc2021.04
  (:require [clojure.string :as s]
            [clojure.set :as st]))

(def data
  (-> (slurp "2021/data/04") (s/split #"\n\n")))

(def drawn-numbers
  (-> data first (s/split #",")))

(defn- parse-board [board-str]
  (let [xform (comp (map s/trim) (map #(s/split % #"\s+")))]
    (->> board-str s/split-lines (into [] xform))))

(def boards
  (->> data rest (map parse-board)))

(defn columns [board]
  (apply map vector board))

(defn solved? [board numbers]
  (let [numbers (set numbers)
        match (fn [line] (st/subset? (set line) numbers))]
    (or (some match board)
        (some match (columns board)))))

(defn calc-checksum [solution numbers]
  (->> solution
        (apply concat)
        set
        (#(st/difference % (set numbers)))
        (#(do (println %) %))
        (map #(Integer/parseInt %))
        (apply +)
        (* (-> numbers last Integer/parseInt))))

(defn solve-walk
  ([boards numbers] (solve-walk boards numbers 1))
  ([boards numbers idx]
   (let [drawn-numbers (take idx numbers)
         solution (->> boards (filter #(solved? % drawn-numbers)) first)]
      (if (nil? solution)
        (recur boards numbers (inc idx))
        (calc-checksum solution drawn-numbers)))))

(defn solve-walk2
  ([boards numbers] (solve-walk2 boards numbers 1))
  ([boards numbers idx]
   (let [drawn-numbers (take idx numbers)
         unsolved (->> boards (filter #((complement solved?) % drawn-numbers)))]
      (if (zero? (count unsolved))
        (calc-checksum (last boards) (take idx numbers))
        (recur unsolved numbers (inc idx))))))

(solve-walk2 boards drawn-numbers)
