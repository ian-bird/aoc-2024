(ns day-nineteen 
  (:require
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(def valid-pattern?
  (e/mrfn [towels pattern]
          (if (= pattern "")
            true
            (if (->> towels
                     (filter #(str/starts-with? pattern %))
                     (map #(subs pattern (count %)))
                     (some (fn [after-cut] (recur towels after-cut))))
              true
              false))))

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
       (map #( apply valid-pattern? %))
       (filter identity)
       count
       time))