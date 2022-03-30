(use 'clojure.string)
(require '[clojure.core.async :as a])
(require '[clojure.core.match :refer [match]])
(require '[instaparse.core :as insta])

(defn remove-whitespace [s]
  (join (split s #"\ *")))

(def tokenizer
  (insta/parser
    "EXP =  T+
     T   = LIT | OP | LP | RP
     LIT = #'[0-9]+'
     OP  = '+' | '*'
     LP  = '('
     RP  = ')' "))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(def first-precedences
  { "+" 0
    "*" 0})

(def second-precedences
  { "+" 1
    "*" 0})

(defn add-op [precendes]
  (fn [op-stack output op]
    (let [op-precedence (precedences op)]
      (loop [ops op-stack
             out output]
        (let [top-op (peek ops)
              top-precedence (precedences top-op)]
          (if (and top-op top-precedence (> top-precedence op-precedence))
            (recur (pop ops) (conj out top-op))
            [(conj ops op) out]))))))

(defn right-parens [op-stack output]
  (loop [ops op-stack
         out output]
    (if (= "(" (peek ops))
      [(pop ops) out]
      (recur (pop ops) (conj out (peek ops))))))

(defn tokens [line]
  (->> line remove-whitespace tokenizer (drop 1) (map last)))

(defn shunting-yard [precedences]
  (let [add-op-fn (add-op precedences)]
    (fn [token-stream]
      (loop [[head & tail] token-stream
              op-stack (list)
              output (queue)]
        (match head
            [:OP op]   (let [[new-op-stack new-output] (add-op-fn op-stack output op)]
                        (recur tail new-op-stack new-output))

            [:LIT lit] (recur tail op-stack (conj output (Integer/parseInt lit)))

            [:LP lp]   (recur tail (conj op-stack lp) output)

            [:RP rp]   (let [[new-op-stack new-output] (right-parens op-stack output)]
                        (recur tail new-op-stack new-output))

            nil (into output op-stack))))))

(def ops
  {"*" *'
   "+" +'})

(defn eval-rev-pol [token-queue]
  (loop [[token & tail] token-queue
          stack (list)]
    (if-let [op-fn (ops token)]
      (let [[args rst] (split-at 2 stack)
             new-val (apply op-fn args)]
        (recur tail (conj rst new-val)))
      (if (nil? token)
        (peek stack)
        (recur tail (conj stack token))))))

(defn solve [shunting-yard-fn]
  (->> data
    (pmap (comp eval-rev-pol shunting-yard-fn tokens))
    (reduce +')))

(defn first-solution []
  (let [shunting-yard-fn (shunting-yard first-precedences)]
    (solve shunting-yard-fn)))

(defn second-solution []
  (let [shunting-yard-fn (shunting-yard second-precedences)]
    (solve shunting-yard-fn)))
