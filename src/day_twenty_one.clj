(ns day-twenty-one 
  (:require
   [clojure.string :as str] 
   [extension :as e]))

(defn seq-from-coords
  [oob [fy fx] [ty tx]]
  (->> [(cond (> fy ty) (repeat (- fy ty) 'n)
              (< fy ty) (repeat (- ty fy) 'v)
              :else '())
        (cond (> fx tx) (repeat (- fx tx) '<)
              (< fx tx) (repeat (- tx fx) '>)
              :else '())]
       ((fn [[l1 l2]] [(concat l1 l2) (concat l2 l1)]))
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
       distinct))

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

(def dpad-seq 
  (let [key-coords {'n [0 1]
                    'A [0 2]
                    '< [1 0]
                    'v [1 1]
                    '> [1 2]}]
    (memoize (e/on (partial seq-from-coords [0 0]) key-coords))))

(defn distance
  [& coords]
  (->> coords
       (apply mapv vector)
       (mapv #(abs (apply - %)))
       (apply +)))

(def base-costs
  "number of human button presses to move
   from a robot from one symbol to another one
   and then press it.
   
   < -> > = >>A = cost of 3"
  (let [dirs ['n 'A '< 'v '>]
        key-coords {'n [0 1]
                    'A [0 2]
                    '< [1 0]
                    'v [1 1]
                    '> [1 2]}]
    (->> (e/combinations dirs dirs) 
         (mapcat (fn [[a b]] [[a b] (inc ( (e/on distance key-coords) a b))])) 
         (apply hash-map))))


(defn additive-costs
  "given a cost map, calculate a new cost map for
   a robot controlling that robot.
   e.g., going from < to ^ requires input
   >^A, so the cost would be (+ (cost A >) (cost > ^) (cost ^ A))"
  [cost-map]
  (->> cost-map
       keys
       (mapcat (fn [[from to]] [[from to]
                                (->> (dpad-seq from to)
                                     (map #(concat '(A) % '(A)))
                                     (map e/zip-with-rest)
                                     (map #(map reverse %))
                                     (map (fn [seq]
                                            (reduce + 0 (map cost-map seq))))
                                     (apply min))]))
       (apply hash-map)))

(defn extract-num
  [string]
  (->> (str/split string #"")
       (drop-while (partial = "0"))
       (filter #(re-matches #"[0-9]+" %))
       str/join
       read-string))

(defn complexity
  [string]
  (let [costs (reduce (fn [x _] (additive-costs x))
                      base-costs
                      (repeat (dec 25) nil))]
    (->> (str/split (str "A" string) #"")
         e/zip-with-rest
         (map reverse)
         (mapv #(map read-string %))
         (map (partial apply numpad-seq))
         (map (fn [seqs]
                (map #(map reverse (e/zip-with-rest (concat '(A) % '(A))))
                     seqs)))
         (map (fn [seqs]
                (apply min (map (fn [seq] (reduce + (map costs seq))) seqs))))
         (reduce + 0)
         (* (extract-num string)))))

(reduce + 0 (map complexity [
                             ; problem set
]))

(reduce + 0 (map complexity [
                             ; test set
]))