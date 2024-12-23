(ns day-twenty-two 
  (:require 
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


(defn get-2000th [s] (reduce (fn [x _] (next-secret x)) s (repeat 2000 nil)))

(let [fn "data/day_twenty_two/problem"]
  (fh/extract-nums-only fn)
  (->> (str fn ".edn")
       slurp
       read-string
       (mapcat identity)
       (pmap get-2000th)
       (reduce +)))