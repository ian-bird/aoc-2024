(ns day-thirteen)

(defn find-answer
  [[a1 a2] [b1 b2] [p1 p2]]
  (let [x (/ (- (* b2 p1) (* b1 p2)) (- (* a1 b2) (* a2 b1)))
        y (/ (- (* a2 p1) (* a1 p2)) (- (* a2 b1) (* a1 b2)))]
    (if (and (int? x) (int? y)) (+ (* 3 x) y) ##Inf)))

(->> "data/day_thirteen/problem.edn"
     slurp
     read-string
     (map (fn [[[ax ay] [bx by] [px py]]] [[ax ay] [bx by]
                                           [(+ px 10000000000000)
                                            (+ py 10000000000000)]]))
     (map #(apply find-answer %))
     (filter (comp not infinite?))
     (reduce +)
     time)