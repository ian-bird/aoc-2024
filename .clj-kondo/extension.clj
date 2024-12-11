(ns extension)

(defn deep-replace 
  [nested-coll old new]
  (map #(if (= % old) new (cond
                            (list? %) (deep-replace % old new)
                            (vector? %) (into [] (deep-replace % old new))
                            :else %))
       nested-coll))

(defmacro mrfn 
  [[& args] body]
  (let [fn# (gensym)]
    `(fn [~@args]
       (let [~fn# (fn [mem-fn# ~@args]
                    (let [~fn# (fn [& mem-args#]
                                 (apply mem-fn# mem-fn# mem-args#))]
                      ~(deep-replace body 'recur fn#)))
             mem-fn# (memoize ~fn#)]
         (mem-fn# mem-fn# ~@args)))))