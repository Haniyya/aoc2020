(use 'clojure.string)

(def sample-str "939
7,13,x,x,59,x,31,19")

(def data-str (slurp "data/input_13"))

(defn parse [data]
  [(-> data split-lines first Integer/parseInt)
   (->> (-> data split-lines last (split #","))
        (filter #(not= % "x"))
        (map #(Integer/parseInt %)))])

(def timestamp (-> data-str split-lines first Integer/parseInt))

(def busses (->> (-> data-str split-lines last (split #","))
                 (filter #(not= % "x"))
                 (map #(Integer/parseInt %))))

(defn closest-multiple [number div]
  (if (zero? (mod number div))
    number
    (let [mul (quot number div)]
      (* (inc mul) div))))

(comment
  (first (sort-by (partial closest-multiple timestamp) busses))
  (* 443
    (- (closest-multiple timestamp 443) timestamp)))

;; Part 2
(defn parse-last [[idx n]]
  [idx (Integer/parseInt n)])

(defn parse2 [data]
  [(-> data split-lines first Integer/parseInt)0\
   (->> (-> data split-lines last (split #","))
        (map-indexed vector)
        (filter (comp #(not= % "x") last))
        (map parse-last))])

(def main-sample (last (parse2 data-str)))

(defn normalize [[offset div]]
  (if (> offset div)
    (normalize [(- offset div) div])
    [(- div offset) div]))

(defn congruence-class [[remainder modn]]
  (map #(-> % (* modn) (+ remainder)) (range)))

(defn congruent? [[offset div] k]
  (= offset (mod k div)))

(defn sieve [c-seq congruency]
  (let [nmod (last congruency)
        seq-step (->> (take 2 c-seq) (apply -) (Math/abs))
        start (first (filter (partial congruent? congruency) c-seq))
        new-step (*' nmod seq-step)]
    (iterate #(+' % new-step) start)))

(defn solve [parsed-numbers]
  (let [[head & tail] (map normalize parsed-numbers)]
    (first (reduce sieve (congruence-class head) tail))))

(comment
  (solve main-sample))
