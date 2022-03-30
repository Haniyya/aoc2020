(defn memory [starters turns]
  (loop [number (last starters)
         turns-left (- turns (count starters))
         occurrences (into {} (map vector (butlast starters) (rest (range))))]
    (if (zero? turns-left)
      number
      (let [current-turn (- turns turns-left)
            new-occ (assoc occurrences number current-turn)]
        (if-let [occurrence (get occurrences number)]
          (let [spoken (- current-turn occurrence)]
           (recur spoken (dec turns-left) new-occ))
          (recur 0 (dec turns-left) new-occ))))))

(comment
  (memory [11 18 0 20 1 7 16] 2020)
  (memory [11 18 0 20 1 7 16] 30000000))
