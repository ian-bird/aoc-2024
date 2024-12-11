(ns extension)

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
   Recusion can be done via mrecur in much the same way as recur,
   though this *DOES NOT* implement tail recursion.
   
   This is a way to create recursive memoized functions, useful
   for things such as the fibonacci sequence, to avoid exponential
   time complexity."
  [[& args] body]
  (let [fn# (gensym)]
    `(fn [~@args]
       (let [~fn# (fn [mem-fn# ~@args]
                    (let [~fn# (fn [& mem-args#]
                                 (apply mem-fn# mem-fn# mem-args#))]
                      ~(deep-replace body 'mrecur fn#)))
             mem-fn# (memoize ~fn#)]
         (mem-fn# mem-fn# ~@args)))))