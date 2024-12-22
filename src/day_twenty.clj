(ns day-twenty 
  (:require
   [day-sixteen :as d16]
   [day-eighteen :as d18]
   [extension :as e]
   [file-help :as fh]))



(defn distance [[x1 y1] [x2 y2]] (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn between
  [[x1 y1] [x2 y2]]
  (cond (> x1 x2) (between [x2 y1] [x1 y2])
        (> y1 y2) (between [x1 y2] [x2 y1])
        :else (->> (range x1 (inc x2))
                   (map (fn [x] (map (fn [y] [x y]) (range y1 (inc y2)))))
                   (mapcat identity)
                   (e/reject (fn [coords] (or (= coords [x1 y1]) (= coords [x2 y2]))))
                   (take 2))))

(defn path->individual-costs
  [path end]
  (loop [path path
         result {end 0}
         max-in-result 0]
    (if (every? #(result %) path)
      result
      (recur path
             (let [edges (filter #(= max-in-result (result %)) path)]
               (merge result
                      (apply hash-map
                             (interleave
                              (->> path
                                   (filter (fn [considering]
                                             (some #(= 1 (distance considering %))
                                                   edges)))
                                   (e/reject #(contains? result %)))
                              (cycle [(inc max-in-result)])))))
             (inc max-in-result)))))

(defn shortcut-squares
  [[s1 s2]]
  (between s1 s2))

(with-redefs [d16/step-left d18/step-left
              d16/step-right d18/step-right]
  (let [fp "data/day_twenty/problem"]
    (fh/extract-chars fp)
    (let [maze (->> (str fp ".edn")
                    slurp
                    read-string)
          maze-solution (d16/find-paths maze)
          end-row (first (keep-indexed (fn [i row]
                                         (some #(if (= "E" %) i false) row))
                                       maze))
          end-col (first (keep-indexed (fn [i v] (if (= "E" v) i nil))
                                       (maze end-row)))
          path (conj (maze-solution :path) [end-row end-col])
          costs (path->individual-costs path [end-row end-col])
          shortcuts (->> path
                         (e/outer* #(vector %1 %2) path)
                         (mapcat identity)
                         (map sort)
                         (filter #(= 2 (apply distance %)))
                         ; we need the bricks for each and are going to
                         ; distinct on that
                         (e/chunk shortcut-squares)
                         (map (fn [options]
                                (->> options
                                     (map (fn [[s1 s2]]
                                            (- (abs (- (costs s1) (costs s2)))
                                               2)))
                                     (apply max)))))]
      (count (filter (partial <= 100) shortcuts)))))