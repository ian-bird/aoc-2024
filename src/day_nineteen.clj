(ns day-nineteen 
  (:require
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(def count-valid-patterns
  (e/mrfn [towels pattern]
          (if (= pattern "")
            1
            (->> towels
                 (filter #(str/starts-with? pattern %))
                 (map #(subs pattern (count %)))
                 (map (fn [after-cut] (recur towels after-cut)))
                 (reduce + 0)))))

(let [fp "data/day_nineteen/problem"]
  (fh/txt->edn fp
               (fn [lines]
                 (->> lines
                      (mapv (fn [line]
                              (if (str/includes? line ",")
                                (->> (str/split line #",")
                                     (map str/trim)
                                     sort
                                     (into []))
                                line)))
                      (filter #(e/!= % "")))))
  (->> (str fp ".edn")
       slurp
       read-string
       ((fn [data] (map (fn [line] [(first data) line]) (rest data)))) 
       (pmap #( apply count-valid-patterns %))
       (reduce + 0)
       time))