(ns day-five 
  (:require
   [clojure.string :as str]
   [extension :as e]
   [file-help :as fh]))

(fh/txt->edn "data/day_five/problem_rules"
                    (fn [element]
                      (->> element
                           (map #(str/split % #"\|"))
                           (map #(map read-string %)))))

(fh/txt->edn "data/day_five/problem_updates"
                    (fn [element]
                      (->> element
                           (map #(str/split % #","))
                           (map #(map read-string %)))))

; p1


(defn matched-rules
  [an-update rules]
  (filter #(and (e/contains? (first %) an-update)
                (e/contains? (second %) an-update)) rules))

(defn rule-followed? [rule, an-update]
  (< (e/index-of-pred (partial = (first rule)) an-update)
     (e/index-of-pred (partial = (second rule))
                              an-update)))

(defn ok? [rules an-update]
  (->> rules
       (matched-rules an-update)
       (e/all? #(rule-followed? % an-update))))

(defn failed-rules
  [rules an-update]
  (->> rules
       (matched-rules an-update)
       (filter (comp not #(rule-followed? % an-update)))))

(defn center-val [coll] (nth coll (/ (count coll) 2)))

(let [updates (read-string (slurp "data/day_five/problem_updates.edn"))
      rules (read-string (slurp "data/day_five/problem_rules.edn"))]
  (binding []
    (->> updates
         (filter (partial ok? rules))
         (map center-val)
         (reduce + 0))))


; very slow, this could be optimized
(defn fix
  [rules update failed-rule]
  (let [unmatched-rule-elements ; return the elements of the failed rule
                                ; that didnt match the value passed in
        #(filter (fn [e] (not (= e %))) failed-rule)
        with-rule-appled ; take the rule that failed and swap the position
                         ; of those 2 values in the array
        ( e/scan (fn [_ element]
                  (if (= (count (unmatched-rule-elements element)) 1)
                    (first (unmatched-rule-elements element))
                    element))
                nil
                update)]
    (if (ok? rules with-rule-appled)
      with-rule-appled
      (fix rules
           with-rule-appled
           (first (failed-rules rules with-rule-appled))))))



; p2
(let [updates (read-string (slurp "data/day_five/problem_updates.edn"))
      rules (read-string (slurp "data/day_five/problem_rules.edn"))
      failed-updates (filter #(not (ok? rules %)) updates)]
  (->> failed-updates
       (map (partial failed-rules rules))
       ( map first)
       (e/zip failed-updates) 
       (map #(fix rules (first %) (second %)))
       (map center-val)
       (reduce + 0)))
