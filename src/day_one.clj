(ns day-one
  (:require
   [clojure.string :as str]
   [extension]))


; convert to edn
(->> "data/day_one/problem.txt"
     slurp
     str/split-lines
     (map #(str/split % #" "))
     (map #(map read-string %))
     pr-str
     (spit "data/day_one/problem.edn"))

; solution for p1
(->> "data/day_one/problem.edn"
     slurp
     read-string
     (apply mapv vector)
     (map sort)
     (apply map list)
     (map #(abs (- (first %) (second %))))
     (reduce + 0))

; solution for p2
(let [input (->> "data/day_one/problem.edn"
                slurp
                read-string)
      ; update the map to have all the numbers in it
      right-counts (->> input
                        (extension/chunk #(second %))
                        (map #(vector (second (first %)) (count %)))
                        (into {}))]
  (->> input
       (filter #(contains? right-counts (first %)))
       (map #(* (first %) (get right-counts (first %))))
       (reduce + 0)))
