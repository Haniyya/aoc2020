(use 'clojure.string)
(require '[instaparse.core :as insta])
(require '[clojure.core.match :refer [match]])

(def rules
  (split-lines
    (first
      (split (slurp "data/input_19") #"\n\n"))))

(def data
  (split-lines
    (second
      (split (slurp "data/input_19") #"\n\n"))))

(def rule-parser
  (insta/parser
    "rule = num <':'> branch (<' |'> branch)*
     num = #'[0-9]+'
     branch = (<' '> token)+
     token = num | literal
     literal = <'\"'> #'[a-z]' <'\"'>"))

(def parse-rule
  (partial insta/parse rule-parser))

(def parsed-rules
  (->> rules (map parse-rule)))

(def rule-index
  (->> parsed-rules (map rcomp2) (reduce merge)))

(def test-rules
  "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"")

(def test-data
  "ababbb
bababa
abbbab
aaabbb
aaaabbb")

(defn compile-regex [tree rule-index]
  (match tree
    [:or branches] (if (= 1 (count branches))
                     (compile-regex (first branches) rule-index)
                     (str "(" (join "|" (mapv #(compile-regex % rule-index) branches)) ")"))
    [:and clauses] (->> clauses (mapv #(compile-regex % rule-index)) join)
    [:ref number]  (compile-regex (get rule-index number) rule-index)
    [:= lit] lit))

(def test-rule-index
  (->> test-rules
       split-lines
       (map parse-rule)
       (map rcomp2)
       (reduce merge)))

(comment
  (let [rules rule-index
        zero-node (get rules "0")
        filter-fn (partial re-matches (re-pattern (compile-regex zero-node rules)))]
    (->> data  (filter filter-fn) count)))
