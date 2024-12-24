(ns day-twenty-two 
  (:require 
   [extension :as e]
   [file-help :as fh]))

(def mix bit-xor)

(defn prune
  [x](mod x 16777216))

(defn next-secret
  [secret]
  (let [step1 (prune (mix secret (* 64 secret)))
        step2 (prune (mix (int (/ step1 32)) step1))
        step3 (prune (mix (* 2048 step2) step2))]
    step3))

(defn prices [s] (->> (repeat 2000 nil)
                        (e/scan (fn [x _] (next-secret x)) s) 
                           (concat (list s))
                        (map #(mod % 10))))

(defn sell-sequences
  [s]
  (let [p (prices s)]
    (->> (range 0 5)
         (map #(repeat % nil))
         (map #(reduce (fn [x _] (rest x)) p %))
         (apply e/zip)
         (map (fn [seq]
                (list (map #(apply - %) (e/zip-with-rest seq)) (last seq)))) 
         (e/chunk first)
         (map last)
         (mapcat identity)
         (apply hash-map))))

(let [fn "data/day_twenty_two/problem"]
  (fh/extract-nums-only fn)
  (->> (str fn ".edn")
       slurp
       read-string
       (mapcat identity)
       (pmap sell-sequences)
       (reduce #(merge-with + %1 %2)) 
       (apply max-key val)
       time))