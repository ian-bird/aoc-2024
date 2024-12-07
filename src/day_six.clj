(ns day-six 
  (:require
   [clojure.string :as str]
   [file-help :as fh]
   [extension :as e]))

(fh/txt->edn "data/day_six/test" (fn [lines] (map #(str/split % #"") lines)))



; p1

(defn find-path
  [pos dir tiles]
  (let [fast-lookup (memoize #(e/nD-nth % tiles))
        rotate-right-sym {"^" ">" ">" "V" "V" "<" "<" "^"}
        dirs-to-steps {"^" [-1 0] "V" [1 0] "<" [0 -1] ">" [0 1]}]
    (loop [steps (list pos) dir dir]
      (let [next-spot (->> dir
                           (get dirs-to-steps)
                           (e/zip (first steps))
                           (map #(apply + %)))
            tile-value (fast-lookup next-spot)]
        (if (= tile-value "#")
          (recur steps (get rotate-right-sym dir))
          (if ( = tile-value "!")
            steps
            (recur (cons next-spot steps) dir)))))))

(defn solve-p1 [tiles]
  (let [pos (e/nD-index-of-pred #(e/contains? % (str/split "^V<>" #"")) tiles)
        dir (e/nD-nth pos tiles)]
    (-> pos (find-path dir tiles) distinct count)))

(let [tiles (-> "data/day_six/test.edn"
                slurp
                read-string)]
(solve-p1 tiles))
  
(fh/txt->edn "data/day_six/problem" (fn [lines] (map #(str/split % #"") lines)))

(let [tiles (-> "data/day_six/problem.edn"
                slurp
                read-string)]
  (solve-p1 tiles))

; p2
(defn loop?
  [pos dir tiles insert-position]
  (let [fast-lookup (memoize #(e/nD-nth % tiles))
        rotate-right-sym {"^" ">" ">" "V" "V" "<" "<" "^"}
        dirs-to-steps {"^" [-1 0] "V" [1 0] "<" [0 -1] ">" [0 1]}]
    (loop [steps (list pos)
           dir dir
           visited #{}]
      (let [next-spot (->> dir
                           (get dirs-to-steps)
                           (e/zip (first steps))
                           (map #(apply + %)))
            tile-value (fast-lookup next-spot)
            set-key (list (first steps) dir)]
        (if (contains? visited set-key)
          true
          (if (or (= tile-value "#") (= next-spot insert-position))
            (recur steps (get rotate-right-sym dir) (conj visited set-key))
            (if (= tile-value "!")
              false
              (recur (cons next-spot steps) dir (conj visited set-key)))))))))

(defn solve-p2
  [tiles]
  (let [pos (e/nD-index-of-pred #(e/contains? % (str/split "^V<>" #"")) tiles)
        dir (e/nD-nth pos tiles)]
    (->> tiles
         (find-path pos dir)
         distinct
         (pmap #(loop? pos dir tiles %))
         (filter identity)
         count)))

(let [tiles (-> "data/day_six/test.edn"
                slurp
                read-string)]
  (solve-p2 tiles))

(let [tiles (-> "data/day_six/problem.edn"
                slurp
                read-string)]
  (solve-p2 tiles))