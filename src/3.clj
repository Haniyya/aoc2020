(use 'clojure.string)

(def data
  (split-lines
   (slurp "data/input_3")))

(def forest (map #(cycle %) data))

(defn in-forest [coordinates]
  (reduce #(nth %1 %2 nil) forest coordinates))

(defn step [[y x]]
  [(inc y) (+ x 3)])

(def step-by (partial mapv +))

(defn path [step] (iterate (partial step-by step) [0 0]))

(defn trees-on-path [step]
  (->> (path step) (map in-forest) (take-while some?) (filter (partial = \#)) count))

(let [directions [[1 1]
                  [1 3]
                  [1 5]
                  [1 7]
                  [2 1]]]
  (->> (map trees-on-path directions) (reduce *)))

