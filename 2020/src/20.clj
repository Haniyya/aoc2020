(require '[clojure.string :as s]
         '[clojure.core.logic :as l :refer [fresh run  run* membero appendo defnu defne]]
         '[clojure.core.logic.pldb :as p]
         '[clojure.core.logic.fd :as fd])

(def data
  (slurp "data/input_20"))

(def tiles-raw
  (-> data (s/split #"\n\n")))

(defn parse-tile [[header lines]]
  (let [id (->> header first (re-find #"\d+") Integer/parseInt)
        lines (->> lines (map seq))]
    [id lines]))

(->> "Tile 23542:\n" (re-find #"\d+") Integer/parseInt)

(def rotate reverse)

(defn flip [lines]
  (mapv reverse lines))

(def tiles
  (->> tiles-raw
       (mapv s/split-lines)
       (mapv #(split-at 1 %))
       (mapv parse-tile)))

(declare cornerso)
(declare valido)


;; Util
(l/defnu reverso [l r]
  ([[] []])
  ([[a] [a]])
  ([[head . tail] _]
   (l/fresh [rb]
    (l/appendo rb [head] r)
    (reverso tail rb))))

;; Border extraction

(defnu line-mapo [lines rel b]
  ([[] _ []])
  ([[head . tail] _ [bhead . btail]]
   (rel head bhead)
   (line-mapo tail rel btail)))

(defn lasto [coll l]
  (fresh [butl]
    (appendo butl [l] coll)))

(defn left-bordero [lines b]
  (line-mapo lines l/firsto b))

(defn right-bordero [lines b]
  (line-mapo lines lasto b))

(def top-bordero l/firsto)
(def lower-bordero lasto)

(defn bordero [lines borders]
  (fresh [n e s w]
    (top-bordero lines n)
    (right-bordero lines e)
    (lower-bordero lines s)
    (left-bordero lines w)
    (l/== borders [n e s w])))

(comment
  (let [lines (->> tiles first last)]
    (run* [q] (lower-bordero lines q))))

;; Transformations
(defne rotate-zo [borders z t]
  ([x 0 x])
  ([[n e s w] 180 [tn w ts e]]
   (reverso n tn)
   (reverso s ts)))

(defne rotate-xo [borders x t]
  ([x 0 x])
  ([[n e s w] 180 [s te n tw]]
   (reverso e te)
   (reverso w tw)))

(defne rotate-yo [borders y t]
  ([x 0 x])
  ([[n e s w] _ _]
   (fresh [ny nt ts tn]
      (l/== nt [e ts w tn])
      (reverso s ts)
      (reverso n tn)
      (fd/+ ny 90 y)
      (rotate-yo nt ny t))))

(defne transformo [borders orientation t]
  ([_ [x y z] _]
   (fresh [tx ty]
      (rotate-xo borders x tx)
      (rotate-yo tx y ty)
      (rotate-zo ty z t))))

(comment
  (let [b (map seq ["Nnn" "Eee" "Sss" "Www"])]
    (run* [q]
        (transformo b [180 90 180] q))))

(defn oriento [o]
  (fresh [x y z]
    (fd/dom x (fd/domain 0 180))
    (fd/dom y (fd/domain 0 90 180 270))
    (fd/dom z (fd/domain 0))
    (l/== [x y z] o)))

(defn varianto [tile]
  (fresh [orig orientation lines oborders tborders id]
    (membero orig tiles)
    (l/firsto orig id)
    (oriento orientation)
    (lasto orig lines)
    (bordero lines oborders)
    (transformo oborders orientation tborders)
    (l/== tile [id tborders])))

(comment
  (count
    (run* [q] (varianto q))))

;; matchers

(defne match [tile dir neighbor]
  ([[n _ _ _] :n [_ _ n _]])
  ([[_ e _ _] :e [_ _ _ e]])
  ([[_ _ s _] :s [s _ _ _]])
  ([[_ _ _ w] :w [_ w _ _]]))

(comment
  (run* [q]
    (match [1 2 3 4] :n q)
    (match [5 6 7 8] :e q)
    (match [9 10 11 12] :s q)
    (match [13 14 15 16] :w q)))

(defne match-tile [tile dir neighbor]
  ([[_ tborders] _ [_ nborders]]
   (match tborders dir nborders)))

;; Board validation

(defn height [lvars]
  (-> lvars count Math/sqrt int))

(defn south-neigbors [lvars]
  (let [lheight (height lvars)]
    (->> lvars
         (partition lheight)
         (partition 2 1)
         (mapcat #(map list (first %) (last %))))))

(comment
  (south-neigbors (range 0 144)))

(defn east-neighbors [lvars]
  (let [lheight (height lvars)]
    (->> lvars
         (partition lheight)
         (mapcat #(partition 2 1 %)))))

(comment
  (east-neighbors (range 0 144)))

;; Board construction

(l/defne lineso [tile lines]
  ([[_ lines]]))

(l/defne ido [tile id]
  ([[id _]]))

(l/defne producto [factors product]
  ([[] 1])
  ([[a . tail] _]
   (l/fresh [c]
      (fd/* a c product)
      (producto tail c))))

(p/db-rel variant [id borders])

(let [borders
      '[(\# \. \# \# \. \. \. \. \. \#)
        (\# \. \. \. \# \# \# \# \. \#)
        (\. \# \# \# \# \. \. \# \# \#)
        (\# \# \. \. \# \# \. \. \. \.)]]
  (run* [q]
    (match borders :e borders)
    (l/== q borders)))

(defn positiono [mtile ltile]
  (fresh [orientation]
    (oriento orientation)
    (l/matche [mtile ltile]
      ([[id borders] [id tborders]]
       (transformo borders orientation tborders)))))

(defne not-membero [x l]
  ([_ []])
  ([_ [head . tail]]
   (l/!= x head)
   (not-membero x tail)))

(defn positions
  ([tiles ltiles] (positions tiles ltiles (l/lvar)))
  ([tiles ltiles seen]
   (l/matche [ltiles]
    ([[]])
    ([[thead . ttail]]
     (fresh [mtile nseen]
            (membero mtile tiles)
            (not-membero mtile seen)
            (membero mtile nseen)
            (positiono mtile thead)
            (positions tiles ttail nseen))))))

(comment
  (let [vars (repeatedly 12 (l/lvar))]
    (run 1 [q]
      (positions tiles vars)
      (l/== q vars))))

(defn solution []
  (let [ltiles (repeatedly (count tiles) l/lvar)
        ids (repeatedly (count ltiles) l/lvar)
        ens (east-neighbors ltiles)
        sns (south-neigbors ltiles)]
      (l/run 1 [q]
        (l/everyg (fn [[tile tile2]] (match-tile tile :e tile2)) ens)
        (l/everyg (fn [[tile tile2]] (match-tile tile :s tile2)) sns)
        (l/everyg (fn [tile] (variant tile)) ltiles)
        ;(l/everyg (fn [[tile id]] (ido tile id)) (zipmap ltiles ids))
        ;(l/distincto ids)
        (l/== ltiles q))))

(comment
  (solution))
