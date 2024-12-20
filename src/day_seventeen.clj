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
         b (biginteger b)
         c (biginteger c)
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
        0 (recur (int (/ a (math/pow 2 combo))) b c next out)
        1 (recur a (bit-xor  b literal) c next out)
        2 (recur a (mod combo 8) c next out)
        3 (if (= a 0) (recur a b c next out) (recur a b c literal out))
        4 (recur a (.xor b c) c next out)
        5 (recur a b c next (conj out (mod combo 8)))
        6 (recur a (int (/ a (math/pow 2 combo))) c next out)
        7 (recur a b (int (/ a (math/pow 2 combo))) next out)
        nil out))))

(defn fix-reg-a
  [_ b c instructions]
  ; lazy sequence magic
  (->> (range)
       (pmap (fn [a] [(compute a b c instructions) a]))
       (filter #(= instructions (first %)))
       first
       second))

(let [maze-file "data/day_seventeen/test"]
  (fh/extract-nums-only maze-file)
  (->> (e/strcat maze-file ".edn")
       slurp
       read-string
       (map (fn [c] (if (> (count c) 1) c (first c))))
       (apply compute) 
       time))