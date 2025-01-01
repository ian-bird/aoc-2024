(ns day-one
  (:require
   [clojure.string :as str]
   [extension]))


; convert to edn,
; just extracting out the numbers
; and putting them into a 2d array
(->> "data/day_one/problem.txt"
     slurp
     str/split-lines
     (map #(str/split % #" "))
     (map #(remove (partial = "") %))
     (mapv #(mapv read-string %))
     pr-str
     (spit "data/day_one/problem.edn"))

; solution for p1
(->> "data/day_one/problem.edn"
     slurp
     read-string
     (apply mapv vector)
     (map sort)
     (apply mapv vector)
     (map #(abs (- (% 0) (% 1))))
     (reduce +))

; solution for p2
(let [input (->> "data/day_one/problem.edn"
                slurp
                read-string)
      ; create a map of numbers to how many times they appear
      right-counts (->> input
                        (reduce (fn [map [l r]] (update map r #(cons l %))) {})
                        (#(update-vals % count)))]
  (->> input
       (filter #(contains? right-counts (% 0)))
       (map #(* (% 0) (right-counts (% 0))))
       (reduce +)))