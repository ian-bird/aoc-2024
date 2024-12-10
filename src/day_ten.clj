(ns day-ten 
  (:require
    [file-help :as fh]))

(fh/extract-chars "data/day_ten/problem")

(defn valid-step?
  [top-map this-value r c]
  (= (inc this-value)
     (-> top-map
         (nth r)
         (nth c))))

(defn trace
  [top-map r c]
  (let [this-value (-> top-map
                       (nth r)
                       (nth c))
        valid? (fn [r c]
                      (valid-step? top-map this-value r c))]
    (if (< this-value 9)
      (concat
       (if (and (> r 0) (valid? (dec r) c)) (trace top-map (dec r) c) '())
       (if (and (> c 0) (valid? r (dec c))) (trace top-map r (dec c)) '())
       (if (and (< (inc r) (count top-map)) (valid? (inc r) c))
         (trace top-map (inc r) c)
         '())
       (if (and (< (inc c) (count (first top-map))) (valid? r (inc c)))
         (trace top-map r (inc c))
         '()))
      (list (list r c)))))

(defn trace2
  [top-map r c]
  (let [this-value (-> top-map
                       (nth r)
                       (nth c))
        valid? (fn [r c] (valid-step? top-map this-value r c))]
    (if (< this-value 9)
      (+ (if (and (> r 0) (valid? (dec r) c)) (trace2 top-map (dec r) c) 0)
         (if (and (> c 0) (valid? r (dec c))) (trace2 top-map r (dec c)) 0)
         (if (and (< (inc r) (count top-map)) (valid? (inc r) c))
           (trace2 top-map (inc r) c)
           0)
         (if (and (< (inc c) (count (first top-map))) (valid? r (inc c)))
           (trace2 top-map r (inc c))
           0))
      1)))

(let [top-map (map (fn [line] (map read-string line))
                   (read-string (slurp "data/day_ten/problem.edn")))
      zeros (->> top-map
                 (map-indexed
                  (fn [row-num r]
                    (map-indexed (fn [col-num v] (list v row-num col-num)) r)))
                 (apply concat)
                 (filter (comp (partial = 0) first))
                 (map rest))]
  ; p1
  (reduce +
          (map #(count (distinct (trace top-map (first %) (second %)))) zeros))
  ; p2
  (reduce + (map #(trace2 top-map (first %) (second %)) zeros)))