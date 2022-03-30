(require '[clojure.core.logic :refer :all])
(require '[clojure.core.logic.fd :as fd])
(use 'clojure.string)

(def data
  (sort
   (map #(Integer/parseInt %)
        (split-lines
         (slurp "data/input_1")))))

(defn reduceo [init f l target]
  (fresh [a d acc-remaining]
         (conde
          [(== l ()) (== target init)]
          [(conso a d l)
           (f a acc-remaining target)
           (reduceo init f d acc-remaining)])))

(def producto (partial reduceo 1 fd/*))
(def sumo (partial reduceo 0 fd/+))

(defn expense-product [n]
  (let [vars (lvars n)]
    (run 1 [q]
         (everyg #(membero % data) vars)
         (everyg #(fd/< % 2020) vars)
         (sumo vars 2020)
         (producto vars q))))

(expense-product 2)
(expense-product 3)
