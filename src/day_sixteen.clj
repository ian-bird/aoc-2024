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

(def ^:dynamic *maze* nil)

(def ^:dynamic *visited* nil)

(defn- do-trace
  [[current-row current-col] [rel-row rel-col]]
  (cond (or (>= current-row (count *maze*))
            (>= current-col (count (first *maze*))))
        nil
        :else
        (let [this-char (-> *maze*
                            (nth current-row)
                            (nth current-col))
              left (case [rel-row rel-col]
                     [0 1] [1 0]
                     [1 0] [0 -1]
                     [0 -1] [-1 0]
                     [-1 0] [0 1])
              right (case [rel-row rel-col]
                      [0 1] [-1 0]
                      [-1 0] [0 -1]
                      [0 -1] [1 0]
                      [1 0] [0 1])]
          (cond (= this-char "E") 0
                (contains? *visited* [current-row current-col]) nil
                (= this-char "#") nil
                :else
                (loop [score 1
                       r current-row
                       c current-col
                       sub-visited #{}]
                  (if (and (-> *maze*
                               (nth (+ r (first left)))
                               (nth (+ c (second left)))
                               (= "#"))
                           (-> *maze*
                               (nth (+ r (first right)))
                               (nth (+ c (second right)))
                               (= "#"))
                           (-> *maze*
                               (nth (+ r rel-row))
                               (nth (+ c rel-col))
                               (= ".")))
                    (recur (inc score) (+ r rel-row) (+ c rel-col) (conj sub-visited [r c]))
                    (binding [*visited* (set/union *visited*
                                               #{ [r c]}
                                               sub-visited)]
                      (min-ignoring-nils
                       (list (+without-nils
                              1000
                              score
                              (do-trace [(+ r (first left))
                                         (+ c (second left))]
                                        left))
                             (+without-nils
                              1000
                              score
                              (do-trace [(+ r (first right))
                                         (+ c (second right))]
                                        right))
                             (+without-nils score
                                            (do-trace
                                             [(+ r rel-row)
                                              (+ c rel-col)]
                                             [rel-row rel-col])))))))))))

  
  (def do-trace (memoize do-trace))
 
(defn trace-path
  [maze score [current-row current-col] [rel-row rel-col] previously-visited]
  (binding [*maze* maze 
            *visited* previously-visited]
    (do-trace [current-row current-col] [rel-row rel-col])))

 (defn p1
  [maze]
  (let [s-row (e/index-of-pred #(e/contains? "S" %) maze)
        s-col (e/index-of-pred (partial = "S") (nth maze s-row))] 
    (trace-path maze 0 [s-row s-col] [0 1] #{})))

 (let [maze-file "data/day_sixteen/problem"]
  (fh/extract-chars maze-file)
  (->> (e/strcat maze-file ".edn")
       slurp
       read-string 
       p1
       time))