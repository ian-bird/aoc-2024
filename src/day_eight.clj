(ns day-eight 
  (:require
    [file-help :as fh]
    [extension :as e]))

(fh/extract-chars "data/day_eight/test")

(fh/extract-chars "data/day_eight/problem")

(defn antenna-groups
  [input]
  (->> input
       (map-indexed (fn [r line] (map-indexed (fn [c e] (list e r c)) line)))
       (map (fn [line] (filter #(e/!= (first %) ".") line)))
       (apply concat)
       (e/chunk first)
       (map #(map rest %))))

(defn pairs
  [input]
  (->> input 
       antenna-groups
       (map (fn [group] (e/outer* #(list %1 %2) group group)))
       (apply concat)
       (apply concat)
       (filter #(e/!= (first %) (second %)))))

(defn anti-node [pair-of-locations]
  (->> pair-of-locations
       (apply e/zip)
       (map #(- (* 2 (first %)) (second %)))))

(defn anti-nodes
  [pair-of-locations]
  (let [diff (reduce (partial map -) pair-of-locations)]
    (concat (list (first pair-of-locations))
            (e/scan (fn [acc _] (map + diff acc))
                    (first pair-of-locations)
                    (range 0 150)))))

(let [input (read-string (slurp "data/day_eight/problem.edn"))
      bound (count input)
      location-pairs (pairs input)
      cleanup (fn [pairs]
                (->> pairs
                     (filter (fn [l] (e/all? #(and (<= 0 %) (> bound %)) l)))
                     distinct
                     count))]
  ; p1
  (->> location-pairs
       (map anti-node)
       cleanup)
  ;p2
  (->> location-pairs
       (map anti-nodes)
       (apply concat)
       cleanup))