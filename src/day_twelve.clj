(ns day-twelve 
  (:require
   [extension :as e]
   [file-help :as fh]))

(fh/extract-chars "data/day_twelve/test")

(defn adjacent?
  "returns true if two elements are adjacent.
   Determine by getting absolute different of row
   and column and adding together. To be adjacent
   the sum must be exactly 1."
  [e1 e2]
  (= 1
     (+ (abs (- (get e1 'col) (get e2 'col)))
        (abs (- (get e1 'row) (get e2 'row))))))

(adjacent?  {'col 2, 'num-adjacent 5, 'row 2, val "R"}
            {'col 3, 'num-adjacent 5, 'row 2, val "R"})

(defn plots->gardens
  [plots]
  (reduce (fn [gardens plot]
            (let [adjacent-gardens (filter (fn [garden]
                                             (e/any? #(and  ( adjacent? plot %) (= (get % 'val) (get plot 'val)))
                                                     garden))
                                           gardens)
                  other-gardens (filter #(not (e/contains? % adjacent-gardens))
                                        gardens)]
              (cons (cons plot (apply concat adjacent-gardens)) other-gardens)))
          '()
          plots))

(defn num-adjacent [plot plots] (count (filter (partial adjacent? plot) plots)))

; concept - split into groups, get the number of adjacent elements for each plot in the garden
; subtract the number of adjacents from 4, yielding the count of adjacent empty slots
; then get cost by summing that up and multiplying it by the total count of plots
(->> "data/day_twelve/problem.edn"
     slurp
     read-string
     (map-indexed
      (fn [row-num row]
        (map-indexed (fn [col-num v] {'val v 'row row-num 'col col-num}) row)))
     (mapcat identity)
     plots->gardens
     (map (fn [garden]
            (map (fn [plot] (- 4 (num-adjacent plot garden))) garden)))
     (map #(* (count %) (reduce + 0 %)))
     (reduce +))

(defn num-sides
  [garden]
  (let [order (fn [primary secondary]
                (->> garden
                     (e/chunk #(get % primary))
                     (sort-by #(get (first %) primary))
                     (map (partial sort-by #(get % secondary)))
                     (map (fn [subset] (map #(get % secondary) subset)))))
        scans (list (order 'row 'col)
                    (reverse (order 'row 'col))
                    (order 'col 'row)
                    (reverse (order 'col 'row)))]
    (->> scans
         (map (fn [scan] (e/zip-with-rest (cons '() scan))))
         ; go through each tuple from the zip-with-rest
         ; and discard the elements that appear in the "prev"
         ; part of the tuple.
         (map (fn [scan]
                (map (fn [tuple]
                       (filter (fn [new-v]
                                 (not (e/contains? new-v (second tuple))))
                               (first tuple)))
                     scan)))
         ; merge them all together
         (mapcat identity)
         ; now we're going to do another zip-with-reset
         ; to find distinct groups of lines,
         ; by subtracting the tuples and then discarding 1s,
         ; leaving us only with non-adjacent line segments
         ; on the scan
         (filter (comp not empty?))
         (map (partial cons -2))
         (map e/zip-with-rest)
         (map (fn [tuples] (map #(apply - %) tuples)))
         (mapcat identity)
         (filter (partial e/!= 1))
         ; now just count those up
         count)))

(->> "data/day_twelve/problem.edn"
     slurp
     read-string
     (map-indexed
      (fn [row-num row]
        (map-indexed (fn [col-num v] {'val v 'row row-num 'col col-num}) row)))
     (mapcat identity)
     plots->gardens
     (map #(* ( num-sides %) (count %)))
     (reduce +))