(ns day-one
  (:require
   [extension]))

; solution for p1
(let [input (read-string (slurp "data/day_one.edn"))]
  (reduce +
          0
          (map #(abs (- (first %) (second %)))
               (extension/zip (sort (map first input)) (sort (map second input))))))

; solution for p2
(let [input (read-string (slurp "data/day_one.edn"))
      ; create a map of all the nums in the right column with 0 in each slot
      zeroed-map (reduce (fn [map e] (assoc map (second e) 0)) {} input)
      ; update the map to have all the numbers in it
      right-counts
      ( reduce (fn [map e] (update map (second e) inc)) zeroed-map input)]
  ( reduce (fn [sum e]
             ; for each thing in the left column,
             ; add it times the number of times it appears in the right column
             ; or zero if its not in the map
            (+ sum
               (if (contains? right-counts (first e))
                 (* (first e) (get right-counts (first e)))
                 0)))
          0
          input))

 