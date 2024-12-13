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

(defn zip [& lists] (apply (partial map list) lists))

(defn append [l1 & lrest] (apply concat (cons l1 lrest)))

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
  (loop [s s
         coll coll
         result []]
    (if (>= s (count coll))
      (conj result coll)
      (recur s (drop s coll) (conj result (take s coll))))))

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

(defn rotate
  "shifts n elements from the end of the array to the front"
  [n coll]
  (if (> n 0)
    (rotate (dec n) (concat (list (last coll)) (drop-last coll)))
    (if (< n 0)
      (rotate (inc n) (concat (rest coll) (list (first coll))))
      coll)))

(defn combinations
  "returns a list of lists of the possible combinations from 2 lists
   (may expand this to work with n lists)"
  [coll1 coll2]
  (->> coll1
       (map (fn[e1](map #(list e1 %) coll2)))
       (apply concat)))

(defn n-combinations
  [& colls]
  ( reduce (fn [acc e] (map flatten (combinations acc e)))  colls))

(defn index-of-pred
  "returns the first index of a value that satisfies the predicate"
  [pred coll]
  (->> coll
       (map-indexed list)
       (filter (comp pred second))
       ffirst))

(index-of-pred (partial = 3) '(1 2 4))

(defn nD-index-of-pred
  "returns a list corresponding to the path to a value that satisfies the predicate"
  [pred coll]
  (->> coll
       (map-indexed list)
       (filter #(if (coll? (second %))
                  (not (nil? (first (nD-index-of-pred pred (second %)))))
                  (pred (second %))))
       first
       (#(if (coll? (second %))
           (cons (first %) (nD-index-of-pred pred (second %)))
           (list (first %))))))

(defn nD-nth
  "recursive nth for multi-dimensional arrays"
  [path coll]
  (if (= '() path) coll (nD-nth (rest path) (nth coll (first path)))))

(defn update
  "passes old value at n to f, returns the list with only that value changed"
  [n f coll]
  (loop [n n f f coll coll res '()]
  (if (= n 0)
    (concat (reverse (cons (f (first coll)) res)) (rest coll))
    (recur (dec n) f (rest coll) (cons  (first coll) res)))))

(defn nD-update
  "path is an Nth dimensional index, replace the value at that position passing it through f"
  [path f coll]
  (if (-> path count (= 1))
    (update (first path) f coll)
    (update (first path) #(nD-update (rest path) f %) coll)))

(defn nD-inBounds
  "returns whether or not the position corresponds to an existant value"
  [path coll]
  (if (= path '())
    true
    (if (or (> 0 (first path)) (<= (count coll) (first path)))
      false
      (nD-inBounds (rest path) (nth coll (first path))))))


(defn pfilter
  "parallelized version of filter"
  [f coll]
  (->> coll
       (pmap f)
       (zip coll)
       (filter second)
       (map first)))

(defn <*>
  "applicative operator"
  [outer left right]
  (fn[l r](outer (left l) (right r))))

(defn dup
  [f]
  (fn[x](f x x)))

(defn on
  [outer both]
  (<*> outer both both))

(defn !=
  [x1 x2 & xs]
  (not (apply = (cons x1 (cons x2 xs)))))

(defn outer*
  "performs the outer product, returning a 2d array
   and applying the function at each index" 
  [f coll1 coll2]
  (map (fn [c1] (map (fn [c2] (f c1 c2)) coll2)) coll1))

(defn deep-replace
  "recursively replace occurences of the old thing with the new 
   thing. The comparison is done before descent, so replacing 
   collections works."
  [nested-coll old new]
  (map #(if (= % old) new (cond 
                            (list? %) (deep-replace % old new) 
                            (vector? %) (into [] (deep-replace % old new))
                            :else %))
       nested-coll))

(defmacro mrfn
  "This macro behaves similar to fn, except it memoizes its results.
   Recusion can be done via recur, though this *DOES NOT* implement 
   tail recursion, nor need to be called in the tail position.
   
   This is a way to create recursive memoized functions, useful
   for things such as the fibonacci sequence, to avoid exponential
   time complexity." 
  [[& args] body] 
  (let [fn# (gensym)]
    `(fn [~@args]
       (let [~fn# (fn [mem-fn# ~@args]
                    (let [~fn# (fn [& mem-args#]
                                 (apply mem-fn# mem-fn# mem-args#))]
                      ~(deep-replace body 'recur fn#)))
             mem-fn# (memoize ~fn#)]
         (mem-fn# mem-fn# ~@args)))))

(defn zip-with-rest [coll]
  "zip a collection with (rest collection).
   this creates a collection of tuples of items
   that were adjacent in the original collection.
   This is useful for measuring increases across a collection."
  (zip (rest coll) coll))