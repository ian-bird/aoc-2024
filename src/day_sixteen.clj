(ns day-sixteen
  (:require
   [clojure.set :as set]
   [extension :as e]
   [file-help :as fh]))

(defn step-forward
  [from dir from-cost path]
  {(mapv + from dir) {:dir dir :cost (+ 1 from-cost) :path (conj path from)}})

(defn step-left
  [from dir from-cost path]
  (let [left (case dir
               [0 1] [1 0]
               [1 0] [0 -1]
               [0 -1] [-1 0]
               [-1 0] [0 1])]
    {(mapv + from left)
     {:dir left :cost (+ 1001 from-cost) :path (conj path from)}}))

(defn step-right
  [from dir from-cost path]
  (let [right (case dir
                [0 1] [-1 0]
                [-1 0] [0 -1]
                [0 -1] [1 0]
                [1 0] [0 1])]
    {(mapv + from right)
     {:dir right :cost (+ 1001 from-cost) :path (conj path from)}}))

(defn steps-forward
  [square hash]
  (mapv #(apply (eval %) square (map (partial get hash) [:dir :cost :path]))
        (list step-forward step-left step-right)))

(defn steps-backwards
  [square dir]
  (let [coords (fn [[abs-r abs-c] [rel-r rel-c]] [(+ abs-r rel-r) (+ abs-c rel-c)])
        back-back (map (partial * -2) dir)
        back-left (assoc (mapv (partial * -1) dir) 0 1)
        back-right (assoc (mapv (partial * -1) dir) 0 -1)]
    (map (partial coords square) [back-back back-left back-right])))

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
            {:dir [0 1] :cost 0 :path #{}}}]
      (if (contains? visited-squares end)
        (get visited-squares end)
        (->> visited-squares
             ; get the squares
             keys
             ; get all the squares that can be walked to from
             ; each visited square
             (mapcat (fn [visited-square]
                       (steps-forward visited-square
                                      (get visited-squares visited-square))))
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
             ; get the cheapest moves; we're only doing these
             (get-mins #(get (first (vals %)) :cost))
             ; do these have more than one valid path to them? if so union
             ; their paths add them to the visited list before recurring
             (map
              (fn [hash]
                (let [from (first (keys hash))
                      metadata (first (vals hash))
                      this-cost (get metadata :cost)
                      this-dir (get metadata :dir)
                      potential-routes-from (steps-backwards from this-dir)
                      target-cost-diffs [2 1002 1002]
                      matched (mapv vector
                                    potential-routes-from
                                    (mapv (partial - this-cost)
                                          target-cost-diffs))]
                  (->> matched
                       ; only get the ones with valid costs for backtracing
                       (filter (fn [possible-backstep]
                                 (= (get (get visited-squares
                                              (first possible-backstep))
                                         :cost)
                                    (second possible-backstep))))
                       ; grab the path form it
                       (map (fn [[square _]]
                              (set/union #{square}
                                         (get (get visited-squares square)
                                              :path))))
                       ; build the new path with the merger of the two previous ones
                       (cons p)
                       (apply set/union)
                       (fn [p])
                       (update v :path)
                       (fn [v])
                       (update-vals hash)))))
             ; and record metadata
             (apply merge visited-squares)
             (recur))))))

(let [maze-file "data/day_sixteen/test"]
  (fh/extract-chars maze-file)
  (->> (e/strcat maze-file ".edn")
       slurp
       read-string
       find-paths
       (#(list (get % :cost) (inc (count (get % :path)))))
       time))