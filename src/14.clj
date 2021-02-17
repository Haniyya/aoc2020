(refer-clojure :exclude '[==])
(require '[clojure.string :refer :all :exclude [reverse]])

(defn parse-ins [line]
  (if-let [[_ loc value] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    [:mem (BigInteger. loc) (BigInteger. value)]
    (let [[_ mask] (re-find #"mask = (\w+)" line)]
      [:mask mask])))

(defn parse-program [strinput]
  (->> (split-lines strinput) (map parse-ins)))

(def data
  (parse-program (slurp "data/input_14")))

(defn apply-mask-char [mch vch]
  (case mch
    \X vch
    \1 \1
    \0 \0))

(def bin-length 36)

(defn pad-zeroes [binary]
  (let [padding (repeat (- bin-length (.length binary)) \0)]
    (str (join padding) binary)))

(defn dec->bin [dec-value]
  (pad-zeroes (.toString dec-value 2)))

(defn bin->dec [binary]
  (BigInteger. binary 2))

(defn apply-mask [mask dec-value]
  (->
    (map apply-mask-char mask (dec->bin dec-value))
    join
    bin->dec))

(defn execute [[mask memory :as state] [ins & args]]
  (case ins
    :mem (let [[loc value] args] (assoc-in state [1 loc] (apply-mask mask value)))
    :mask (let [[value & _] args] (assoc state 0 value))))

(def initial-state
  [nil {}])

(defn solve [exec-fn]
  (reduce +
    (vals
      (get
        (reduce exec-fn initial-state data)
        1))))

(comment
  (solve execute))

;; Part 2

(use 'clojure.core.logic)

(defn bind-lvar [mch vch logic-var]
  (case mch
    \X (membero logic-var [0 1])
    \1 (== logic-var 1)
    \0 (== logic-var vch)))

(defn bind-all [mask binary vars]
  (and* (map bind-lvar (seq mask) binary vars)))

(defn all-locations [decimal mask]
  (let [binary (seq (dec->bin decimal))
        vars (repeatedly (count binary) lvar)]
   (map (comp bin->dec join)
    (run* [q]
      (== q vars)
      (bind-all mask binary vars)))))

(defn execute2 [[mask memory :as state] [ins & args]]
  (case ins
    :mem (let [[loc value] args] (reduce #(assoc-in %1 [1 %2] value) state (all-locations loc mask)))
    :mask (let [[value & _] args] (assoc state 0 value))))

(comment
  (solve execute2))
