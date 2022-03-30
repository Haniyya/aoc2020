(use 'clojure.string)

(defn parse-ins [ins]
  (let [[dir number] (split-at 1 ins)]
    [(first dir) (Integer/parseInt (join number))]))

(def start-ship
  [\E [0 0]]) ; Dir, [North, East]

(defn parse-course [course-str]
  (->>
    course-str
    split-lines
    (map trim)
    (map parse-ins)))

(def data (parse-course (slurp "data/input_12")))

(defn turn [dir degrees]
  (let [directions (seq "NESW")
        idx (index-of "NESW" dir)
        nidx (mod (+ idx (/ degrees 90)) 4)]
    (nth directions nidx)))

(defn dir->update [[dir number]]
  (case dir
    \R [[0] turn number]
    \L [[0] turn (- number)]
    \N [[1 0] + number]
    \S [[1 0] - number]
    \E [[1 1] + number]
    \W [[1 1] - number]))

(defn forward-update [[dir _] number]
  (dir->update [dir number]))

(defn ins->update [ship [dir number]]
  (if (= dir \F)
    (forward-update ship number)
    (dir->update [dir number])))

(defn apply-ins [ship ins]
  (let [[ks f number] (ins->update ship ins)]
    (update-in ship ks f number)))

(comment
  (->>
    (reduce apply-ins start-ship data)
    last
    (map #(Math/abs %))
    (reduce +)))

;; Part 2

(def start-ship2
  [[1 10] [0 0]])

(defn int-angle [f degrees]
  (-> degrees Math/toRadians f Math/round))

(def sin (partial int-angle #(Math/sin %)))
(def cos (partial int-angle #(Math/cos %)))

(defn rotate [[waypoint pivot] degrees]
  (let [new-waypoint
        (mapv + pivot
          (let [[x y] (mapv - waypoint pivot)]
            [(- (* x (cos degrees)) (* y (sin degrees)))
             (+ (* x (sin degrees)) (* y (cos degrees)))]))]
    [new-waypoint pivot]))

(defn forward [[waypoint pos] number]
  (let [distance (mapv - waypoint pos)
        new-pos (mapv + pos (mapv #(* number %) distance))]
    [(mapv + distance new-pos) new-pos]))

(defn ship-update [ship [dir number]]
  (case dir
    \R [rotate number]
    \L [rotate (- number)]
    \F [forward number]))

(defn waypoint-update [[dir number]]
  (case dir
    \N [0 + number]
    \S [0 - number]
    \E [1 + number]
    \W [1 - number]
    nil))

(defn drive [ship ins]
  (if-let [[pos f arg] (waypoint-update ins)]
    (update-in ship [0 pos] f arg)
    (let [[f arg] (ship-update ship ins)]
      (f ship arg))))

(comment
  (->>
    (reduce drive start-ship2 data)
    last
    (map #(Math/abs %))
    (reduce +)))
