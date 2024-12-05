(ns day-four 
  (:require
   [clojure.string :as str]
   [extension]))

; we can analyze most of this by searching for xmas across
; the strings, the transpose of the strings, and the reverse of each.

; convert to 2D array of chars
(->> "data/day_four_test.txt"
     slurp
     str/split-lines
     (map #(str/split % #""))
     pr-str
     (spit "data/day_four_test.edn"))

; p1
(let [list-list (->> "data/day_four.edn"
                     slurp
                     read-string
                     (map #(concat % (list "|")))) ; putting this in to stop diagonal matches wrapping
      horizontal (map str/join list-list)
      transpose (fn [x] ; finds transpose, allowing all matches to be done on a single axis
                  (->> x
                       (map (partial into '()))
                       (apply mapv vector)
                       (map str/join)))
      vertical (transpose list-list)
      diagonalify (fn [x] ; rotates all the arrays appropriately then transposes
                    (->> x
                         (extension/zip list-list)
                         (map #(apply (extension/flip extension/rotate) %))
                         transpose))
      tlbr-diagonal (->> list-list
                         (extension/scan (fn [a _] (inc a)) -1)
                         diagonalify)
      trbl-diagonal (->> list-list
                         (extension/scan (fn [a _] (dec a)) 1)
                         diagonalify)
      match (fn [re str-list] ; finds if the regex is in the string and returns the count
              (->> str-list
                   (map #(re-seq re %))
                   (map count)
                   (reduce +)))
      match-both #(+ (match #"XMAS" %) (match #"SAMX" %))]
  (->> (list horizontal vertical tlbr-diagonal trbl-diagonal)
       (map match-both)
       (reduce +)))

; p2

; we're going to do this one by creating all the possible
; squares that we need to analyze first, then going through them
; and seeing if they match.
(let [; this is our 2D array of chars
      list-list (->> "data/day_four.edn"
                     slurp
                     read-string
                     (map #(concat % (list "|"))))
      ; start by zip-with-rest'ing horizontally to get the groups
      ; of 3 at each point, trimming off the ones at the edges
      ; that won't work
      three-chars (->> list-list
                       (map #(extension/zip % (rest %) (rest (rest %)))))
      ; then zip-with-rest vertically, which gives us a 3x3 char array
      ; at each point a char was originally.
      char-squares-on-grid (->> three-chars
                                (extension/zip (rest (rest three-chars))
                                               (rest three-chars))
                                (map #(apply map list %)))
      ; this is our function to test if a char grid matches the XMAS
      ; pattern.
      is-xmas?
      (fn [square]
        (let [valid-corners '("M" "S") ; only m or s can appear in the
                                         ; corners
              valid-center '("A") ; only a can appear in the center
                ; corners are the combinations of first and last for both
                ; axes
              corner-positions (extension/combinations '(first last)
                                                       '(first last))
                ; take the combinations and apply them to the square
                ; to get the actual values in the corners
              corners (map #(apply (eval (first %))
                                   (list (apply (eval (second %)) (list square))))
                           corner-positions)]
            ; to be a valid xmas, all the corners must be valid, the center
            ; must be valid, and the diagonally opposing corners cannot
            ; match.
          (and (extension/all? #(extension/contains? % valid-corners) corners)
               (extension/contains? (second (second square)) valid-center)
               (not (= (first corners) (last corners)))
               (not (= (second corners) (second (rest corners)))))))]
  ; get the 2D array of 3x3 squares,
  ; turn it into a 1D array of 3x3 squares,
  ; filter out only the ones that are xmas,
  ; count them up
  (->> char-squares-on-grid
       (apply concat)
       (filter is-xmas?)
       (count)))
