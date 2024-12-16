(ns day-fourteen 
  (:require
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(fh/extract-nums-only "data/day_fourteen/problem")

(def width 101)
(def height 103)

(defn move
  [seconds robot-info-list]
  (map (fn [v] [(mod (+ (nth v 0) (* seconds (nth v 2))) width)
                (mod (+ (nth v 1) (* seconds (nth v 3))) height)])
       robot-info-list))

(->> "data/day_fourteen/problem.edn"
     slurp
     read-string
     (move 100)
     (map (fn [[x y]] (let [x-off (- x (/ (dec width) 2))
                            y-off (- y (/ (dec height) 2))]
                        (cond
                          (and (< x-off 0) (< y-off 0))
                          1
                          (and (> x-off 0) (< y-off 0))
                          2
                          (and (< x-off 0) (> y-off 0))
                          3
                          (and (> x-off 0) (> y-off 0))
                          4
                          :else 0 ))))
     (filter (partial e/!= 0))
     (e/chunk identity) 
     (map count)
     (reduce * 1))

(defn to-string
  [current-poses]
  (str/join "\n" (map (fn [row-num]
         (str/join (map (fn [col-num]
                          (let [c (count (filter #(= [col-num row-num] %)
                                                 current-poses))]
                            (if (= c 0) "." (pr-str c))))
                        (range width))))
       (range height))))



(doall (map (fn [times] (->> "data/day_fourteen/problem.edn"
                             slurp
                             read-string
                             (move times)
                             to-string
                             (list "\n" (pr-str times))
                             (str/join "\n")
                             ( #(spit "data/day_fourteen/output.txt" % :append true)))) (range 10000)))

