(ns day-twenty-four 
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(defn XOR [a b]
  (if (e/!= a b)
    1
    0))

(defn AND [a b]
  (if (and (= a 1) (= b 1))
    1
    0))

(defn OR [a b]
  (if (or (= a 1) (= b 1))
    1
    0))

(fh/txt->edn "data/day_twenty_four/problem_wires"
             (fn [lines]
               (mapv (fn [line]
                       (mapv read-string
                             (str/split (str/replace line #":" "") #" ")))
                     lines)))

(fh/txt->edn "data/day_twenty_four/problem_gates"
             (fn [lines]
               (map (fn [line]
                      (map read-string
                           (str/split (str/replace line #"->" "") #"\s+")))
                    lines)))


(defmacro create-defs
  [pairs]
  (let [with-ones-to-skip-removed
        (filter #(every? resolve (filter symbol? (second %)))
                (eval pairs))]
    `(do ~@(map (fn [pair] `(def ~@pair)) with-ones-to-skip-removed))))

(->> "data/day_twenty_four/problem_wires.edn"
     slurp
     read-string
     create-defs)

(->> "data/day_twenty_four/problem_gates.edn"
     slurp
     read-string
     (map (fn [[l op r asgn]] `(~asgn (~op ~l ~r))))
     create-defs)

(->> "data/day_twenty_four/problem_gates.edn"
     slurp
     read-string
     flatten
     (map pr-str)
     (filter #(str/starts-with? % "z"))
     sort
     (map symbol)
     (map eval)
     (e/zip (range))
     (filter #(= 1 (second %)))
     (map #(bigint ( math/pow 2.0 (first %))))
     (reduce +))