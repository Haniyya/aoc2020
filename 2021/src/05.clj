(ns aoc2021.05
  (:require [clojure.string :as s]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.walk :as w]))

(def data-parser
  (insta/parser
    "data = line*
     line = pair <' -> '> pair <#'\n?'>
     pair = num <','> num
     num = #'[0-9]+'"))

(def data
  (->> (slurp "2021/data/05")
       data-parser
       (insta/transform {:num #(Integer/parseInt %)
                         :pair vector
                         :line vector
                         :data vector})))

(defn point-range [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn horizontal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn line->points [[[x1 y1] [x2 y2]]]
  (let [xrange (point-range x1 x2)
        yrange (point-range y1 y2)]
    (if (horizontal? [[x1 y1] [x2 y2]])
      (for [x xrange y yrange] [x y])
      (map vector xrange yrange))))

(line->points [[9 7] [7 7]])

(defn count-points [lines]
  (->> lines
       (mapcat line->points)
       frequencies
       (filter (fn [[_ c]] (> c 1)))
       count))
(->> data (filter horizontal?) count-points)
(count-points data)
