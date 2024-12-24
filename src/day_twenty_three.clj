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


(defn another-hop
  [conns-ref conns]
  (->> conns
       (map (fn [[k v]] [k (into {} (map #(vector % (conns-ref %)) v))]))
       (into {})))

(let [conns (->> "data/day_twenty_three/problem.edn"
                 slurp
                 read-string
                 (reduce gen-conns {}))]
  (->> conns
       (another-hop conns)
       (map (fn [[k v]] [k (another-hop conns v)]))
       (mapcat (fn [[k1 v1]]
                 (->> v1
                      (map (fn [[k2 v2]] [k2
                                          (->> v2
                                               (map (fn [[k3 v3]]
                                                      [k3 (contains? v3 k1)]))
                                               (filter (fn [[_ v3]] v3))
                                               (map (fn [[k3 _]] k3)))]))
                      (remove (fn [[_ v2]] (empty? v2)))
                      (mapcat (fn [[k2 v2]] (map #(vector k2 %) v2)))
                      (map #(concat [k1] %)))))
       (map sort)
       distinct
       (map #(map pr-str %))
       (filter (fn [triangle] (some #(str/starts-with? % "t") triangle)))
       count))