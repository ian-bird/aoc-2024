(ns day-three 
  (:require
   [clojure.string :as str]
   [extension]))


(->> "data/day_three/problem.txt"
     slurp
     (re-seq #"mul\((?:\d){1,3},(?:\d){1,3}\)")
     (map #(re-seq #"\d+" %))
     (map #(map read-string %))
     (map #(hash-map 'mul %))
     pr-str
     (spit "data/day_three/p1.edn"))

; solve part 1
(->> "data/day_three/p1.edn"
     slurp
     read-string
     (map #(reduce * 1 (get % 'mul)))
     (reduce + 0))

; create new edn for part 2

(->> "data/day_three/problem.txt"
     slurp
     (re-seq #"(?:mul\((?:\d){1,3},(?:\d){1,3}\))|(?:do\(\))|(?:don't\(\))")
     (map #(if (str/starts-with? % "mul")
             (re-seq #"\d+" %)
             (str/replace % "()" "")))
     (map #(if (coll? %) (map read-string %) (read-string %)))
     (map #(if (coll? %) (hash-map 'mul %) %))
     pr-str
     (spit "data/day_three/p2.edn"))

; solve part 2

(let [input (->> "data/day_three/p2.edn"
                 slurp
                 read-string)]
  (->> input
       (extension/scan (fn [sym e] (if (or (= e 'do) (= e 'don't)) e sym)) 'do)
       (extension/zip input)
       (filter #(map? (first %)))
       (map #(if (= (second %) 'do) (reduce * 1 (get (first %) 'mul)) 0))
       (reduce + 0)))