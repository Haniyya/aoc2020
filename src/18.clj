(use 'clojure.string)
(require '[clojure.core.async :as a])

(defn parse-token [ls]
  (let [[tok & tail] ls]
    (cond
      (nil? tok) [nil (list)]
      (= tok \space) (parse-token (drop-while #(= % \space) tail))
      (Character/isDigit tok) (let [[nums rst] (split-with #(Character/isDigit %) ls)]
                                [(Integer/parseInt (apply str nums)) rst])
      :else [(str tok) tail])))

(defn tokenize [line]
  (let [[token rst] (parse-token (seq line))]
    (if (nil? token)
      (list)
      (lazy-seq (cons token (tokenize rst))))))

(def operation? #{"+" "*"})

(defn infix->rpn [line]
  (let [tokens (tokenize line)]
    (println tokens)
    (loop [[h & tail] tokens
           ops '()
           output []]
      (println output ops)
      (cond
        (int? h) (recur tail ops (conj output h))
        (operation? h) (if (operation? (peek ops))
                        (recur tail (list h) (into output (clojure.core/reverse ops)))
                        (recur tail (cons h ops) output))
        (= "(" h) (recur tail (cons h ops) output)
        (= h ")") (let [[nested-ops parent-ops] (split-with #(not= % "(") ops)]
                    (recur tail (rest parent-ops) (vec (concat output nested-ops))))
        :else (concat output (clojure.core/reverse ops))))))

(def data
  (->> (slurp "data/input_18")
       split-lines))

(def small-nums (a/chan 16 (filter #(> 5 %))))
