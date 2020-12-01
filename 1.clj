(require '[clojure.core.logic :refer :all])
(require '[clojure.core.logic.fd :as fd])
(use 'clojure.string)

(def data
  (map #(Integer/parseInt %)
    (split-lines
      (slurp "data/input_1"))))

(defn first []
  (run 1 [q]
        (fresh [x y]
              (membero x data)
              (membero y data)
              (fd/+ x y 2020)
              (fd/* x y q))))

(defn second []
  (run 1 [q]
        (fresh [x y z sum product]
              (membero x data)
              (membero y data)
              (membero z data)
              (fd/+ x y sum)
              (fd/+ sum z 2020)
              (fd/* x y product)
              (fd/* product z q))))
