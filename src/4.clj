(use 'clojure.string)
(require '[clojure.set :as s])

(def passports
  (split (slurp "data/input_4") #"\n\n"))

(defn fields [passport]
  (split passport #"\s+"))

(defn field-name [field]
  (first
    (split field #":")))

(def required-fields
  #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(def all-keys? (partial s/subset? required-fields))

(defn rev [re] (comp some? (partial re-matches re)))

(defn int-between [low up]
  (comp #(<= low % up) #(Integer/parseInt %)))

(def four-digits (rev #"\d{4}"))

(defmulti validation-fn vector?)
(defmethod validation-fn false [f] f)
(defmethod validation-fn true [f-coll]
  (->> f-coll (map (partial apply every-pred)) (apply some-fn)))

(defn validator [& fs]
  (->> fs (map validation-fn) (apply every-pred)))

(def validations
  [["byr" four-digits (int-between 1920 2002)]
   ["iyr" four-digits (int-between 2010 2020)]
   ["eyr" four-digits (int-between 2020 2030)]
   ["hgt" [[(rev #"\d+cm") (comp (int-between 150 193) first #(split % #"(cm|in)"))]
           [(rev #"\d+in") (comp (int-between 59 76) first #(split % #"(cm|in)"))]]]
   ["hcl" (rev #"\#([0-9]|[a-f]){6}")]
   ["ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
   ["pid" (rev #"[0-9]{9}")]])

(defn validation-map [[field & fs]]
  {field (apply validator fs)})

(def validators
  (->> validations (map validation-map) (reduce merge)))

(defn valid-kv? [[field value]]
  ((get validators field any?) value))

(defn valid? [passport]
  (->> passport fields (map #(split % #":")) (filter valid-kv?) (map first) set all-keys?))

(->> passports (filter valid?) count)
