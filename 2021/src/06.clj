(ns aoc2021.06
  (:require [clojure.string :as s]))

(def data
  (frequencies
    (map #(Integer/parseInt %)
      (s/split "3,4,3,1,2" #","))))

;(defn next-generation [fish-map])

