(use 'clojure.string)

(defn jmp [n]
  (fn [[acc pc]]
    [acc (+ pc n)]))

(defn acc [n]
  (fn [[acc pc]]
    [(+ acc n) (inc pc)]))

(defn noop [[acc pc]]
  [acc (inc pc)])

(defn instruction [[op-str num-str]]
  (let [n (Integer/parseInt num-str)]
    (case op-str
      "nop" noop
      "acc" (acc n)
      "jmp" (jmp n))))

(def tokens
  (->> (slurp "data/input_8")
       split-lines
       (map #(split % #" "))
       (mapv vector (iterate inc 0))
       (into {})))

(defn tokens->program [token-map]
  (into {} (for [[pc ins] token-map] [pc (instruction ins)])))

(def program
  (tokens->program tokens))

(defn run-program [program]
  (let [init-state [0 0]
        no-lines #{}]
    (loop [[acc pc] init-state
           visited-lines no-lines]
      (if (visited-lines pc)
        acc
        (let [ins (get program pc)]
          (recur (ins [acc pc]) (conj visited-lines pc)))))))

(comment
  (run-program program))

;; Solution to part 2

(defn run-program-2 [program]
  (let [init-state [0 0]
        last-pc (count program)
        no-lines #{}]
    (loop [[acc pc] init-state
           visited-lines no-lines]
      (if (or (visited-lines pc) (= pc last-pc))
        [acc (= pc last-pc)]
        (let [ins (get program pc)]
          (recur (ins [acc pc]) (conj visited-lines pc)))))))

(def jump-pcs
  (->> tokens
    (filter (comp (partial = "jmp") first last))))

(defn replace-jmp [token-map [pc [_ n]]]
  (assoc token-map pc ["nop" n]))

(def acc-of-correct-program
  (->> jump-pcs
       (map (partial replace-jmp tokens))
       (map tokens->program)
       (map run-program-2)
       (filter last)
       first))
