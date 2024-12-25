(ns day-twenty-five 
  (:require
    [file-help :as fh]
    [extension :as e]))


(let [file "data/day_twenty_five/problem"]
  (fh/extract-chars file)
  (->> (str file ".edn")
       slurp
       read-string
       (#(concat % [[""]]))
       (partition 8)
       (map #(take 7 %))
       (map (fn [schematic]
              (if (every? #(= "#" %) (first schematic))
                {:lock schematic}
                {:key schematic})))
       (map (fn [schematic]
              (map (fn [[k v]]
                     {k (map (fn [row] (count (filter #(= "#" %) row)))
                             (apply mapv vector v))})
                   schematic)))
       (map first)
       (#(e/outer* vector (filter :key %) (filter :lock %)))
       (mapcat identity)
       (map (fn [[{:keys [key]} {:keys [lock]}]] (map + key lock)))
       (remove (fn [pair] (some #(< 7 %) pair)))
       count))