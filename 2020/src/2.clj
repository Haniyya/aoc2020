(use 'clojure.string)

(defn parse-line [line]
  (let [[_ lower upper ch password] (re-find #"(\d+)-(\d+) (\w): (\w+)" line)]
    [(Integer/parseInt lower) (Integer/parseInt upper) (first ch) (seq password)]))

(def data
  (map parse-line
       (split-lines
        (slurp "data/input_2"))))

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
