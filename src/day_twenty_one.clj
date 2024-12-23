(ns day-twenty-one 
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [extension :as e]))

(defn seq-from-coords
  [oob [fy fx] [ty tx]]
  (->> (concat (cond (> fy ty) (repeat (- fy ty) 'n)
                     (< fy ty) (repeat (- ty fy) 'v)
                     :else '())
               (cond (> fx tx) (repeat (- fx tx) '<)
                     (< fx tx) (repeat (- tx fx) '>)
                     :else '()))
       combo/permutations
       (e/reject (fn [path]
                 (->> path
                      (e/scan (fn [[y x] step]
                                (case step
                                  n [(dec y) x]
                                  < [y (dec x)]
                                  > [y (inc x)]
                                  v [(inc y) x]))
                              [fy fx])
                      (some (partial = oob)))))
       (map #(concat % '(A)))))

(def numpad-seq 
  (let [key-coords {1 [2 0]
                    2 [2 1]
                    3 [2 2]
                    4 [1 0]
                    5 [1 1]
                    6 [1 2]
                    7 [0 0]
                    8 [0 1]
                    9 [0 2]
                    0 [3 1]
                    'A [3 2]}]
    (memoize (e/on (partial seq-from-coords [3 0]) key-coords))))

(def dirpad-seq 
  (let [key-coords {'n [0 1]
                    'A [0 2]
                    '< [1 0]
                    'v [1 1]
                    '> [1 2]}]
    (memoize (e/on (partial seq-from-coords [0 0]) key-coords))))

(defn extract-num
  [string]
  (->> (str/split string #"")
       (drop-while (partial = "0"))
       (filter #(re-matches #"[0-9]+" %))
       str/join
       read-string))

(defn expand-seqs
  [pad cons-a? seqs]
  (->> seqs
       (mapcat (fn [seq]
                 (->> (if cons-a? (cons 'A seq) seq) 
                      e/zip-with-rest
                      (map reverse)
                      (map (partial apply pad))
                      (reduce e/combinations)
                      (map flatten))))
       ((fn [seqs]
          (let [min-count (apply min (map count seqs))]
            (filter #(= min-count (count %)) seqs))))))

(defn complexity
  [input]
  (let [remapped (->> (str/split (str "A" input) #"")
                      e/zip-with-rest
                      (map reverse)
                      (map str/join))]
    (->> remapped
         (pmap (fn [input]
                 (->> (str/split input #"")
                      (map read-string)
                      list
                      ; depressurized
                      (expand-seqs numpad-seq false)
                      ; radiaton
                      (expand-seqs dirpad-seq true)
                      ; cold
                      (expand-seqs dirpad-seq true)
                      first
                      count)))
         (reduce + 0)
         (* (extract-num input)))))

(reduce + 0 (map complexity ["169A" "279A" "540A" "869A" "789A"]))