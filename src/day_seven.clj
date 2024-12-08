(ns day-seven 
  (:require 
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(fh/extract-nums-only "data/day_seven/problem")

(defn possible-2?
  [[target & coll]]
  (if (= 1 (count coll))
    (= target (first coll))
    (or ;see if we can do concatenation here, if we can, check that branch 
     (if (and (not (= target (last coll)))
              ((e/on str/ends-with? pr-str) target (last coll)))
       (possible-2? (into [(read-string (str/join
                                         (drop-last
                                          (count (pr-str (last coll)))
                                          (str/split (pr-str target) #""))))]
                          (drop-last coll)))
       false)
      ; if this number is cleanly divisble then we can divide, so check the
      ; branch
     (if (symbol? target) target false)
     (if (int? (/ target (last coll)))
       (possible-2? (into [(/ target (last coll))] (drop-last coll)))
       false)
      ; aways need to check the subtraction branch
     (possible-2? (into [(- target (last coll))] (drop-last coll))))))

(defn solve-2
  [file]
  (let [input (read-string (slurp file))]
    (->> input
         (e/pfilter possible-2?)
         (reduce #(+ %1 (first %2)) 0))))

(time (solve-2 "data/day_seven/problem.edn"))