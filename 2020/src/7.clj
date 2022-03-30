(require '[clojure.core.logic.pldb :as pldb])

(use 'clojure.core.logic)
(use 'clojure.string)

(pldb/db-rel holds ^:index outer  ^:index inner cnt)

(def data
  (split-lines (slurp "data/input_7")))

(defn parse-rule [rule]
  (if-let [[_ cnt inner] (re-find #"(\d+) (\w+ \w+) bags?" rule)]
    [inner (Integer/parseInt cnt)]))

(defn facts [line]
  (let [[_ outer rules] (re-find #"(\w+ \w+) bags contain (.+)" line)
        parsed-rules (map parse-rule (split rules #","))]
    (->> (split rules #",")
         (keep parse-rule)
         (map #(cons outer %)))))

(def loaded-db
  (apply pldb/db
    (->> data (map facts) (apply concat) (map #(cons holds %)))))

(defn inner-list [bag-name]
  (pldb/with-db loaded-db
    (run* [q]
      (fresh [outer inner cnt]
        (== outer bag-name)
        (holds outer inner cnt)
        (== q [inner cnt])))))

(defn inner-count [bag-name]
  (let [il (inner-list bag-name)
        bag-count (fn [[inner cnt]] (+ cnt (* cnt (inner-count inner))))]
    (if (empty? il)
        0
        (->> il (map bag-count) (reduce +)))))

(comment
  (inner-count "shiny gold"))
