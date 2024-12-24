(ns day-twenty-three 
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [file-help :as fh]))

(fh/txt->edn "data/day_twenty_three/problem"
             (fn [lines]
               (map (fn [line]
                      (mapv read-string (str/split line #"\-")))
                    lines)))


(defn gen-conns
  [hmap [a b]]
  (let [a->b (if (hmap a)
               (update hmap a #(conj % b))
               (assoc hmap a #{b}))]
    (if (a->b b)
      (update a->b b #(conj % a))
      (assoc a->b b #{a}))))


(defn extend-set
  [extending conns]
  (->> conns 
       (map (fn [[add-on v]] (if (set/superset? v extending) (conj extending add-on) extending)))
       distinct))

(extend-set #{4} {4 #{1 2 3 5}, 5 #{1 2 4}})

(defn extend-all-sets
  [extendings conns]
  (distinct (mapcat #(extend-set % conns) extendings)))

(defn fully-extend
  [conns]
  (loop [to-extend (map (fn [[comp _]] #{comp}) conns)]
    (let [extended (extend-all-sets to-extend conns)]
      (if (= extended to-extend)
        extended
        (recur extended)))))


(let [conns (->> "data/day_twenty_three/problem.edn"
                 slurp
                 read-string
                 (reduce gen-conns {}))]
  (str/join "," (sort (into [] (apply max-key count (fully-extend conns))))))