(ns extension 
  (:require
   [clojure.spec.alpha :as s]))



(defn scan
  [f v coll] 
  (->> coll
       (reduce (fn [acc e] (concat acc (list (concat (last acc) (list e)))))
                     '(()))
       rest
       (map (fn [e] (reduce f v e)))))

(defmacro zip [& lists] `(map list ~@lists))



(zip '(1 2 3 4) (cycle [true false]))

(defn append [l1 & lrest] (apply concat (cons l1 lrest)))

(->> '(1 2 3 4 5 6) (zip (cycle [true false])) (filter first) (map second))


(defn any?
  [f coll]
  (->> coll
       (map f)
       (reduce #(or %1 %2) false)))

(defn all? [f coll] (not (any? (complement f) coll)))

(defn contains?
  "this version of contains works properly with all collection types"
  [v coll]
  (extension/any? (partial = v) coll))

(defn all? [f coll] (not (extension/any? (complement f) coll)))

(defn blocks
  "returns a list of lists with length equal to s.
              Example: (blocks 2 '(1 2 3 4 5 6)) => ((1 2) (3 4) (5 6))"
  [s coll]
  (if (>= s (count coll))
    (list coll)
    (cons (take s coll) (blocks s (drop s coll)))))

(defmacro specdef
  "creates a new definition with spec checks on arguments and the return
   allowing for easy creation of runtime-checked code"
  ([name [& spec-args] return-spec body]
   `(specdef ~name "" ~(apply vector spec-args) ~return-spec ~body))
  ([name doc-string [& spec-args] return-spec body]
   (let [args (map first (blocks 2 spec-args))]
     `(defn ~name
        ~doc-string
        ~(apply vector args)
        (if (extension/all? #(s/valid? (second %) (first %))
                            ~(cons 'list
                                   (map #(list 'list (first %) (second %))
                                        (blocks 2 spec-args))))
          (let [~'result ~body]
            (if (s/valid? ~return-spec ~'result) ~'result 'invalid-result))
          'invalid-argument)))))

(defn flip [f] #(f %2 %1))

(defn on [both individual] #(both (individual %1) (individual %2)))

(defn chunk
  "Returns a list of chunks in which the return value of calling f on each element is the same."
  [f coll]
  (vals (reduce (fn [map e] (update map (f e) #(cons e %)))
                (reduce (fn [map e] (assoc map (f e) '())) {} coll)
                coll)))
