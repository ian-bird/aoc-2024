(ns day-twenty-four 
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [extension :as e]
   [file-help :as fh]))

(defn fXOR [a b] (if (e/!= a b) 1 0))

(defmacro XOR
  [a b]
  `(quote (fXOR ~(if (number? (eval a)) a (eval a))
                ~(if (number? (eval b)) b (eval b)))))

(defn fAND [a b] (if (and (= a 1) (= b 1)) 1 0))

(defmacro AND
  [a b]
  `(quote (fAND ~(if (number? (eval a)) a (eval a))
                ~(if (number? (eval b)) b (eval b)))))


(defn fOR [a b] (if (or (= a 1) (= b 1)) 1 0))

(defmacro OR
  [a b]
  `(quote (fOR ~(if (number? (eval a)) a (eval a))
               ~(if (number? (eval b)) b (eval b)))))

(defmacro create-defs
  [pairs]
  `(do ~@(map (fn [pair] `(def ~@pair)) (eval pairs))))


(defmacro create-gate-defs
  [pairs]
  (let [with-ones-to-skip-removed
        (filter #(every? resolve (filter symbol? (second %)))
                (eval pairs))]
    `(do ~@(map (fn [pair] `(def ~@pair)) with-ones-to-skip-removed))))

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

(->> "data/day_twenty_four/problem_wires.edn"
     slurp
     read-string
     create-defs)
 
(->> "data/day_twenty_four/problem_gates.edn"
     slurp
     read-string
     (map (fn [[l op r asgn]] `(~asgn (~op ~l ~r))))
     create-gate-defs)


(defn inc-syms
  [coll]
  `(~'inc-syms
    (quote
     ~(walk/prewalk
       (fn [x]
         (let [num-part (->>
                         x
                         pr-str
                         (drop 1)
                         (apply str)
                         ((fn [s]
                            (if (= (first s) \0) (apply str (drop 1 s)) s)))
                         (read-string))]
           (if (and (symbol? x) (number? num-part))
             (->> num-part
                  inc
                  (str (first (pr-str x)) (if (> 9 num-part) "0" ""))
                  read-string)
             x)))
       coll))))

(defn sort-args
  [nested-coll]
  (if (coll? nested-coll)
    (cons (first nested-coll) (sort-by hash (map sort-args (rest nested-coll))))
    nested-coll))

(def reps
  (->
   (->>
    (inc-syms (list
               'quote
               (sort-args
                '(day-twenty-four/fOR
                  (day-twenty-four/fAND c00 (day-twenty-four/fXOR y01 x01))
                  (day-twenty-four/fAND x01 y01)))))
    (iterate eval)
    (take 44)
    (map (fn [c]
           (->> c
                second
                second)))
    (partition 2 1)
    (map (fn [[[_ l] [_ [_ _ [_ _ r]]]]] [(sort-args l) r]))
    (mapcat identity)
    (apply hash-map))
   (assoc (sort-args '(day-twenty-four/fAND y00 x00)) 'c00)
   (assoc (sort-args '(day-twenty-four/fOR
                      (day-twenty-four/fAND c00 (day-twenty-four/fXOR y01 x01))
                      (day-twenty-four/fAND x01 y01)))
          'c01)))

(defn repeatedly-replace
  [smap coll]
  (let [rep (walk/prewalk-replace smap coll)]
    (if (= coll rep)
      coll
      (repeatedly-replace smap (sort-args rep)))))
