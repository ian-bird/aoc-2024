(ns day-nine 
  (:require
   [clojure.zip :as zip]
   [file-help :as fh]))

(fh/extract-chars "data/day_nine/test")
(fh/extract-chars "data/day_nine/problem")

; silver
(def start
  (->> "data/day_nine/test.edn"
       slurp
       read-string
       first
       (map read-string)
       (map-indexed #(if (even? %1) (repeat %2 (/ %1 2)) (repeat %2 'empty)))))

(->>
 (loop [result '()
        input (flatten start)
        stack (reverse (flatten start))
        stop-at (count (flatten start))]
   (if (<= stop-at 0)
     result
     (if (= 'empty (first input))
       (if (= 'empty (first stack))
         (recur result input (rest stack) (dec stop-at))
         (recur (cons (first stack) result)
                (rest input)
                (rest stack)
                (- stop-at 2)))
       (recur (cons (first input) result) (rest input) stack (dec stop-at)))))
 reverse
 (map-indexed *)
 (reduce +))

; gold was done in ruby, ran out of time to re-implement in clojure.