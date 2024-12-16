(ns day-fifteen 
  (:require
   [extension :as e]
   [file-help :as fh]
   [clojure.string :as str]))

(defn convert-to-entities
  [char-map]
  (->> char-map
       (map-indexed (fn [row-num row]
                      (map-indexed (fn [col-num v]
                                     (case v
                                       "#" {:wall [row-num col-num]}
                                       "O" {:box [row-num col-num]}
                                       "@" {:robot [row-num col-num]}
                                       nil))
                                   row)))
       (mapcat identity)
       (filter (complement nil?))
       (e/chunk keys)
       (map (fn [entities]
              {(first (keys (first entities))) (mapcat vals entities)}))
       (mapcat identity)
       (mapcat identity)
       (apply hash-map)))

(defn push-box
  [walls boxes box [row col]]
  (loop [check-row (first box)
         check-col (second box)]
    (if (e/contains? [check-row check-col] walls)
      box
      (if (e/contains? [check-row check-col] boxes)
        (recur (+ check-row row) (+ check-col col))
        [check-row check-col]))))

(defn do-move
  [entities move]
  (let [dir (case move
              "<" [0 -1]
              ">" [0 1]
              "^" [-1 0]
              "v" [1 0])
        boxes (get entities :box)
        walls (get entities :wall)
        robot (first (get entities :robot))
        desired-spot (mapv + robot dir)] 
    (cond (e/contains? desired-spot boxes)
          ; try to push the boxes. If pushed forward,
          ; move box to the pushed-to spot
          ; and move robot to where the box was
            (let [pushed-to (push-box walls boxes desired-spot dir)]
              (if (= pushed-to desired-spot)
                entities
                (-> entities
                    (assoc :box (map #(if (= % desired-spot) pushed-to %)
                                  (get entities :box)))
                    (assoc :robot (list desired-spot)))))
          ; if its a wall dont update anything, cant move
          (e/contains? desired-spot walls) entities
          ; if its empty, we can just move the robot there
          :else (assoc entities :robot (list desired-spot)))))

(def warehouse "data/day_fifteen/problem_warehouse")

(def moves "data/day_fifteen/problem_moves")

(fh/extract-chars warehouse)

(fh/extract-chars moves) 


(let [entities (->> (e/strcat warehouse ".edn")
                    slurp
                    read-string
                    convert-to-entities)
      moves (->> (e/strcat moves ".edn")
                 slurp
                 read-string
                 (mapcat identity))]
  (->> moves
       (reduce do-move entities)
       (#(get % :box))
       (map #(+ (* 100 (first %)) (second %)))
       (reduce + 0)))


(defn convert-to-wide-entities
  [char-map]
  (->> char-map
       (map-indexed (fn [row-num row]
                      (mapcat identity
                              (map-indexed
                               (fn [col-num v] 
                                   (case v
                                   "#" (list {:wall [row-num (* 2 col-num)]}
                                             {:wall [row-num (inc (* 2 col-num))]})
                                   "O" (list {:box [row-num (* 2 col-num) (inc (* 2 col-num))]})
                                   "@" (list {:robot [row-num (* 2 col-num)]})
                                   nil))
                               row))))
       (mapcat identity)
       (filter (complement nil?))
       (e/chunk keys)
       (map (fn [entities]
              {(first (keys (first entities))) (mapcat vals entities)}))
       (mapcat identity)
       (mapcat identity)
       (apply hash-map)))

(defn push-wide-box
  ; returns a new set of boxes
  [walls boxes box [row col]]
  (let [maybe-try-pushing (case row
                      0 (list (mapv + box [0 (* 2 col) (* 2 col)]))
                      1 (list (mapv + box [1 1 1])
                              (mapv + box [1 0 0])
                              (mapv + box [1 -1 -1]))
                      -1 (list (mapv + box [-1 1 1])
                               (mapv + box [-1 0 0])
                               (mapv + box [-1 -1 -1])))
        try-pushing (filter #(e/contains? % boxes) maybe-try-pushing)
        after-pushing (->> try-pushing
                           (reduce #(push-wide-box walls %1 %2 [row col]) boxes)
                           (filter #(e/!= box %)))
        desired-spot (mapv + box [row col col])] 
    (cond (e/any? #(e/contains? % after-pushing ) maybe-try-pushing)
          boxes
          (or (e/contains? [(first desired-spot ) (second desired-spot)] walls)
              (e/contains? [(first desired-spot) (nth desired-spot 2)] walls))
          boxes
          :else (cons desired-spot after-pushing))))

(defn do-wide-move
  [entities move]
  (let [dir (case move
              "<" [0 -1]
              ">" [0 1]
              "^" [-1 0]
              "v" [1 0])
        boxes (get entities :box)
        shattered-boxes (mapcat #(list [(first %) (second %)]
                                       [(first %) (nth % 2)]) boxes)
        walls (get entities :wall)
        robot (first (get entities :robot))
        desired-spot (mapv + robot dir)]
    (cond (e/contains? desired-spot shattered-boxes)
            ; try to push the boxes. If pushed forward, move box to the
            ; pushed-to spot and move robot to where the box was
          (let [specific-box (filter #(or (= desired-spot [(first %) (second %)])
                                          (= desired-spot  [(first %) (nth % 2)]))
                                     boxes)
                pushed-boxes (push-wide-box walls boxes (first specific-box) dir)
                didnt-push (= pushed-boxes boxes)]
            (if (or (empty? specific-box) didnt-push)
              entities
              (-> entities
                  (assoc :box pushed-boxes)
                  (assoc :robot (list desired-spot)))))
          ; if its a wall dont update anything, cant move
          (e/contains? desired-spot walls) entities
          ; if its empty, we can just move the robot there
          :else (assoc entities :robot (list desired-spot)))))

(let [entities (->> (e/strcat warehouse ".edn")
                    slurp
                    read-string
                    convert-to-wide-entities)
      moves (->> (e/strcat moves ".edn")
                 slurp
                 read-string
                 (mapcat identity))]
  

  (->> moves
       (reduce do-wide-move entities)
       (#(get % :box))
       (map #(+ (* 100 (first %)) (second %)))
       (reduce + 0)))
