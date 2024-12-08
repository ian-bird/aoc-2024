(ns day-eight 
  (:require
    [file-help :as fh]
    [extension :as e]))

(fh/extract-chars "data/day_eight/problem")

(defn anti-nodes
  [pair-of-locations]
  (let [diff (reduce (partial map -) pair-of-locations)]
    (concat (list (first pair-of-locations))
            (e/scan (fn [acc _] (map + diff acc))
                    (first pair-of-locations)
                    (range 0 150)))))

(let [input (read-string (slurp "data/day_eight/problem.edn")) ]
  (->> input
       (map-indexed (fn [r line] (map-indexed (fn [c e] (list e r c)) line)))
       (mapcat (fn [line] (filter #(e/!= (first %) ".") line)))
       (e/chunk first)
       (map #(map rest %))
       (mapcat (fn [group] (e/outer* #(list %1 %2) group group))) 
       (apply concat)
       (filter #(e/!= (first %) (second %)))
       (mapcat anti-nodes) 
       (filter (fn [l] (e/all? #(and (<= 0 %) (> (count input) %)) l)))
       distinct
       count))