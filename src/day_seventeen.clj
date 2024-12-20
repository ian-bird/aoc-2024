(ns day-seventeen 
  (:require
   [clojure.math :as math] 
   [extension :as e]
   [file-help :as fh]))

(defn safe-nth
  [coll i]
  (if (or (>= i (count coll))
          (< i 0))
    nil
    (nth coll i)))

(defn compute
  [a b c i]
  (loop [a (biginteger (print-str a))
         b b
         c c
         ip 0
         out []]
    (let [combo (case (safe-nth i (inc ip))
                  nil nil
                  0 0
                  1 1
                  2 2
                  3 3
                  4 a
                  5 b
                  6 c)
          literal (safe-nth i (inc ip))
          next (+ 2 ip)]
      (case (safe-nth i ip)
        0 (recur (biginteger (/ a (math/pow 2 combo))) b c next out)
        1 (recur a (.xor (biginteger b) (biginteger literal)) c next out)
        2 (recur a (mod combo 8) c next out)
        3 (if (= a 0) (recur a b c next out) (recur a b c literal out))
        4 (recur a (.xor (biginteger b) (biginteger c)) c next out) 
        5 (recur a b c next (conj out (mod combo 8)))
        6 (recur a (biginteger (/ a (math/pow 2 combo))) c next out)
        7 (recur a b (biginteger (/ a (math/pow 2 combo))) next out)
        nil out))))

; we can find the sets of 10 bits at a time
; that satisfy 1 output.
; we need to stagger these, i.e.
; when shifted to their correct positions,
; the overlapping sections must all be equal.
; lets start by doing this for all the digits.
(defn fix-reg-a
  [_ b c instructions]
  (->> instructions
       ; each index of this map contains all the 10 bit numbers that
       ; can generate the appropriate value for that position.
       ; now, we just need to overlay them to get the solution.
       (map (fn [instruction]
              (->> (range 1024)
                   (map (fn [v] [(compute v b c instructions) v]))
                   (filter #(= instruction (ffirst %)))
                   (map second))))
       ; we need the index to know how much to bitshift
       (map-indexed vector)
       ; discard the index on the first element so that
       ; the default value for reduce is set correctly
       (map #(if (= 0 (first %)) (second %) %))
       ; check if the top 7 bits of assebmling vals
       ; match the bottom 7 bits of works-for-this vals
       (reduce (fn [previously-workeds works-for-this]
                 (->> works-for-this
                      second
                      (map #(vector (first works-for-this) %))
                      (e/outer* vector previously-workeds)
                      (mapcat identity)
                      (filter (fn [[prev [i this]]]
                                (= (bit-and this 2r1111111)
                                   (bit-shift-right prev (* 3 i)))))
                      (map (fn [[prev [i new]]]
                             (bit-or prev (bit-shift-left new (* 3 i)))))))) 
       (filter #(= instructions (compute % b c instructions )))
       (apply min)))

(let [maze-file "data/day_seventeen/problem"]
  (fh/extract-nums-only maze-file)
  (->> (e/strcat maze-file ".edn")
       slurp
       read-string
       (map (fn [c] (if (> (count c) 1) c (first c))))
       (apply fix-reg-a)
       time))