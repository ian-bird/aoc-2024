(ns day-sixteen
  (:require
   [clojure.set :as set]
   [extension :as e]
   [file-help :as fh]))

(defn min-ignoring-nils
  [vals]
  (if (empty? (filter (complement nil?) vals))
    nil
    (apply min (filter (complement nil?) vals))))

(defn +without-nils
  [& args]
  (if (e/any? nil? args) nil
      (apply + args)))

(defn get-forward
  [from dir from-cost]
  {(mapv + from dir) {:dir dir :cost (+ 1 from-cost)}})

(defn get-left
  [from dir from-cost]
  (let [left (case dir
               [0 1] [1 0]
               [1 0] [0 -1]
               [0 -1] [-1 0]
               [-1 0] [0 1])]
    {(mapv + from left) {:dir left :cost (+ 1001 from-cost)}}))

(defn get-right
  [from dir from-cost]
  (let [right (case dir
                [0 1] [-1 0]
                [-1 0] [0 -1]
                [0 -1] [1 0]
                [1 0] [0 1])]
    {(mapv + from right) {:dir right :cost (+ 1001 from-cost)}}))

(defn get-possible-steps
  [square dir cost]
  (mapv #((eval %) square dir cost) '(get-forward get-left get-right)))

(defn find-paths
  [maze]
  (let [get-mins (fn [by coll]
                   (let [minimum (apply min (map by coll))]
                     (filter #(= (by %) minimum) coll)))
        end [(e/index-of-pred #(e/contains? "E" %) maze)
             (e/index-of-pred
              (partial = "E")
              (nth maze (e/index-of-pred #(e/contains? "E" %) maze)))]]
    (loop [visited-squares
           {[(e/index-of-pred #(e/contains? "S" %) maze)
             (e/index-of-pred
              (partial = "S")
              (nth maze (e/index-of-pred #(e/contains? "S" %) maze)))]
            {:dir [0 1] :cost 0}}]
      (if (contains? visited-squares end)
        (get visited-squares end)
        (->> visited-squares
             ; get the squares
             keys
             ; get all the squares that can be walked to from
             ; each visited square
             (mapcat (fn [visited-square]
                       (get-possible-steps visited-square
                                           (-> visited-squares
                                               (get visited-square)
                                               (get :dir))
                                           (-> visited-squares
                                               (get visited-square)
                                               (get :cost)))))
             ; remove the ones that are out of bounds
             (filter (fn [considering]
                       (let [key (first (keys considering))
                             r (first key)
                             c (second key)]
                         (and (>= r 0)
                              (>= c 0)
                              (< r (count maze))
                              (< c (count (first maze)))))))
             ; remove the ones that are walls
             (filter (fn [considering]
                       (->> considering
                            keys
                            first
                            ((fn [[r c]]
                               (-> maze
                                   (get r)
                                   (get c))))
                            (e/!= "#"))))
             ; remove the ones we've already visited
             (filter (fn [considering]
                       (->> considering
                            keys
                            first
                            (contains? visited-squares)
                            not)))
             (get-mins #(get (first (vals %)) :cost))
             (reduce conj visited-squares)
             (recur))))))



(let [maze-file "data/day_sixteen/test"]
  (fh/extract-chars maze-file)
  (->> (e/strcat maze-file ".edn")
       slurp
       read-string
       find-paths
       (#(list (get % :cost)))
       time))
