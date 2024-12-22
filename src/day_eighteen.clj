(ns day-eighteen 
  (:require
    [file-help :as fh]
   [day-sixteen :as d16]))

(defn step-left
  [from dir from-cost path]
  (let [left (case dir
               [0 1] [1 0]
               [1 0] [0 -1]
               [0 -1] [-1 0]
               [-1 0] [0 1])]
    {(mapv + from left)
     {:dir left :cost (+ 1 from-cost) :path (conj path from)}}))

(defn step-right
  [from dir from-cost path]
  (let [right (case dir
                [0 1] [-1 0]
                [-1 0] [0 -1]
                [0 -1] [1 0]
                [1 0] [0 1])]
    {(mapv + from right)
     {:dir right :cost (+ 1 from-cost) :path (conj path from)}}))

; lets do this by creating an intermediate edn
; that has the requirements
; and then find the shortest path through it
(let [path "data/day_eighteen/problem"
      width 70]
  (fh/extract-nums-only path)
  (->> (str path ".edn")
       slurp
       read-string
       (take 3042)
       (into #{})
       ((fn [dropped]
          (mapv (fn [r]
                  (mapv (fn [c] (cond
                                  (= [0 0] [c r]) "S"
                                  (= [width width] [c r]) "E"
                                  (contains? dropped [c r]) "#"
                                  :else "."))
                        (range (inc width))))
                (range (inc width)))))
       pr-str
       (spit (str path "2.edn")))
  ; redefine step left and step right to
  ; be 1 point instead of 1001
  (with-redefs [d16/step-left step-left
                d16/step-right step-right]
    (->> (str path "2.edn")
         slurp
         read-string
         d16/find-paths
         ( #(get % :cost )))))