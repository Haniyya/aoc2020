(ns aoc2020.02
  (:require [clojure.string :as s]))

(def data
  (->> (slurp "2021/data/02")
       (s/split-lines)
       (map #(s/split % #"\s"))
       (map (fn [[cmd n]] [cmd (Integer/parseInt n)]))))

(defn cmd->pos [[cmd n]]
  (case cmd
    "forward" [n 0]
    "down"    [0 n]
    "up"      [0 (* -1 n)]))

(defn solution1
  (letfn [(vsum [p1 p2] (mapv + p1 p2))]
    (->> data
        (map cmd->pos)
        (reduce vsum [0 0])
        (reduce *))))

(defn cmd->pos2 [[cmd n]]
  (case cmd
    "forward" (fn [[x y a]] [(+ x n) (+ y (* a n)) a])
    "down"    (fn [[x y a]] [x y (+ n a)])
    "up"      (fn [[x y a]] [x y (- a n)])))
