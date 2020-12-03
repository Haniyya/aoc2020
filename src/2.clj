(use 'clojure.core.logic)
(use 'clojure.string)
(require '[clojure.core.logic.fd :as fd])

(defn parse-line [line]
  (let [[_ lower upper ch password] (re-find #"(\d+)-(\d+) (\w): (\w+)" line)]
    [(Integer/parseInt lower) (Integer/parseInt upper) (first ch) (seq password)]))

(def data
  (map parse-line
       (split-lines
        (slurp "data/input_2"))))

(defn sameo [a b diff]
  (conde
   [(== a b) (== diff 1)]
   [(== diff 0)]))

(defn counto [elem l cnt]
  (fresh [h t cnt-of-rest diff]
         (conde
          [(== l ()) (== cnt 0)]
          [(conso h t l)
           (sameo h elem diff)
           (counto elem t cnt-of-rest)
           (fd/+ diff cnt-of-rest cnt)])))

(defn relational-solution []
  (run 10 [q]
       (member1o q data)
       (matcha [q]
               ([[?lower ?upper ?ch ?password]]
                (fresh [ch-cnt]
                       (counto ?ch ?password ch-cnt)
                       (fd/<= ?lower ch-cnt)
                       (fd/>= ?upper ch-cnt))))))

(defn password-valid [[lower upper ch password]]
  (let [cnt (->> password (filter (partial = ch)) count)]
    (<= lower cnt upper)))

(defn password-valid-2 [[pos1 pos2 ch password]]
  (let [pos-correct #(-> password (nth % nil) (= ch))]
    (= 1 (->> [pos1 pos2]
              (map dec)
              (filter pos-correct)
              (count)))))

(defn fun-solution []
  (->> data (filter password-valid)))

(defn fun-solution-2 []
  (->> data (filter password-valid-2)))

(password-valid-2 [1 3 \a (seq "abcde")])
