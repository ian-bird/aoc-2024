(ns day-two 
  (:require
   [clojure.string :as str]
   [extension]))

; convert to edn
(->> "data/day_two/problem.txt"
     slurp
     str/split-lines
     (map #(str/split % #" "))
     (map #(map read-string %))
     pr-str
     (spit "data/day_two/problem.edn"))

; solve p1
(defn safe?
  [coll]
  (let [check-all-diffs #(extension/all?
                          %
                          (->> coll
                               rest
                               (extension/zip coll)
                               (map (fn [e] (- (second e) (first e))))))]
    (and (or (check-all-diffs pos?) (check-all-diffs neg?))
         (check-all-diffs #(>= 3 (abs %))))))


(safe? '(7 6 4 2 1)) ; => true
(safe? '(1 2 7 8 9)) ; => false
(safe? '(1 3 2 4 5)) ; => false

(->> "data/day_two/problem.edn"
    slurp
    read-string
    (filter safe?)
     count)



; solve p2
(defn safe-with-damper?
  [coll]
  (->> coll
       (extension/scan (fn [a _] (inc a)) -1)
       ; get the list with the current index dropped
       (map #(concat (take % coll) (drop (inc %) coll)))
       (extension/any? safe?)))

(safe-with-damper? '(7 6 4 2 1)) ; => true
(safe-with-damper? '(1 2 7 8 9)) ; => false
(safe-with-damper? '(9 7 6 2 1)) ; => false
(safe-with-damper? '(1 3 2 4 5)) ; => true
(safe-with-damper? '(8 5 1 2 1)) ; => true



(->> "data/day_two/problem.edn"
     slurp
     read-string
     (filter safe-with-damper?)
     count)
  