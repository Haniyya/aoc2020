(use 'clojure.string)
(use 'clojure.pprint)

(def data
  (->> (slurp "data/input_11")
       split-lines
       (mapv (comp vec seq))))

(defn adjacents [m lidx cidx]
  (for [x (range (dec lidx) (+ 2 lidx))
        y (range (dec cidx) (+ 2 cidx))
        :when (not= [x y] [lidx cidx])]
    (get-in m [x y] \.)))

(defn seen-adjacents [m lidx cidx]
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not= [x y] [0 0])]
    (letfn [(advance [pos] (mapv + pos [x y]))]
      (->> (iterate advance (advance [lidx cidx]))
          (map #(get-in m % \L))
          (filter #(not= % \.))
          first))))

(defn simulate-field [m lidx cidx field]
  (let [adj (adjacents m lidx cidx)]
    (case field
      \. \.
      \L (if (every? #(not= % \#) adj) \# \L)
      \# (if (->> adj (filter #(= % \#)) count (<= 4)) \L \#))))

(defn simulate-line [m lidx line]
  (map-indexed (partial simulate-field m lidx) line))

(defn simulate [m]
  (->> m
       (map-indexed (partial simulate-line m))
       (mapv vec)))

(defn simulate-field2 [m lidx cidx field]
  (let [adj (seen-adjacents m lidx cidx)]
    (case field
      \. \.
      \L (if (every? #(not= % \#) adj) \# \L)
      \# (if (->> adj (filter #(= % \#)) count (<= 5)) \L \#))))

(defn simulate-line2 [m lidx line]
  (map-indexed (partial simulate-field2 m lidx) line))

(defn simulate2 [m]
  (->> m
       (map-indexed (partial simulate-line2 m))
       (mapv vec)))

(defn occupied-seats [m]
  (->> (flatten m) (filter #(= % \#)) count))

(defn solution [sim-fn]
  (->> (iterate sim-fn data)
      (partition 2 1)
      (filter #(apply = %))
      first
      first
      occupied-seats))

(comment
  (solution simulate)
  (solution simulate2))
