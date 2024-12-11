(ns day-eleven 
  (:require [clojure.string :as str]
            [file-help :as fh]
            [extension :as e]))

(fh/extract-nums-only "data/day_eleven/test")
(fh/extract-nums-only "data/day_eleven/problem")

(defn update-stones
  [stones]
  (mapcat (memoize (fn [stone]
            (let [digits (map read-string (str/split (pr-str stone) #""))
                  num-digits (count digits)]
              (cond (zero? stone) (list 1)
                    (even? num-digits) (list (reduce #(+ (* 10 %1) %2)
                                                     0
                                                     (take (/ num-digits 2) digits))
                                             (reduce #(+ (* 10 %1) %2)
                                                     0
                                                     (drop (/ num-digits 2) digits)))
                    :else (list (* 2024 stone))))))
          stones))

(time (count (reduce (fn [acc _] (update-stones acc))
        (first (read-string (slurp "data/day_eleven/problem.edn")))
        (repeat 25 0))))

; even with memoize this was way too slow. Need to think of a better solution...
; idea -- count number of stones generated from a starting one
; we can fan out and pass the memoized recursive function down to do it.

(def num-stones-spawned
  (e/mrfn [blinks stone]
          (if (>= 0 blinks)
            1
            (let [digits (map read-string (str/split (pr-str stone) #""))
                  num-digits (count digits)]
              (cond (zero? stone) (recur (dec blinks) 1)
                    (even? num-digits)
                    (+ (recur (dec blinks)
                               (reduce #(+ (* 10 %1) %2)
                                       0
                                       (take (/ num-digits 2) digits)))
                       (recur (dec blinks)
                               (reduce #(+ (* 10 %1) %2)
                                       0
                                       (drop (/ num-digits 2) digits))))
                    :else (recur (dec blinks) (* 2024 stone)))))))

(->> "data/day_eleven/problem.edn"
     slurp
     read-string
     first
     (pmap (partial num-stones-spawned 75))
     (reduce +)
     time)