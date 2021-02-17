(require '[clojure.set :as s])
(use 'clojure.string)

(def groups
  (split (slurp "data/input_6") #"\n\n"))

(defn count-answers [group]
  (->> (split-lines group) (map seq) (map set) (apply s/union) count))

(defn count-everyone [group]
  (->> (split-lines group) (map seq) (map set) (apply s/intersection) count))

(->> groups (map count-everyone) (reduce +))
