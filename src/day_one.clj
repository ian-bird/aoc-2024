(ns day-one
  (:require
   [clojure.string :as str]
   [extension]))


; convert to edn
(->> "data/day_one.txt"
     slurp
     str/split-lines
     (map #(str/split % #" "))
     (map #(map read-string %))
     pr-str
     (spit "data/day_one.edn"))

; solution for p1
(->> "data/day_one.edn"
     slurp
     read-string
     (apply mapv vector)
     (map sort)
     (apply map list)
     (map #(abs (- (first %) (second %))))
     (reduce + 0))

; solution for p2
(let [input (read-string (slurp "data/day_one.edn"))
      ; create a map of all the nums in the right column with 0 in each
      ; slot
      zeroed-map (reduce (fn [map e] (assoc map (second e) 0)) {} input)
      ; update the map to have all the numbers in it
      right-counts
      (reduce (fn [map e] (update map (second e) inc)) zeroed-map input)]
  (->> input
       (filter #(contains? right-counts (first %)))
       (map #(* (first %) (get right-counts (first %))))
       (reduce + 0)))
