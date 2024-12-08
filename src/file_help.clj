(ns file-help 
  (:require
   [clojure.string :as str]))

(defn txt->edn
  "passes lines to helper function and then pipes to edn"
  [file-name callback]
  (->> (str/join (list file-name ".txt"))
       slurp
       str/split-lines
       callback
       pr-str
       (spit (str/join (list file-name ".edn")))))

(defn extract-nums-only
  "creates a 2d array of all the numbers and pipes to edn"
  [fpath]
  (txt->edn fpath
               (fn [lines]
                 (->> lines
                      (map (partial re-seq #"\d+"))
                      (mapv #(mapv read-string %))))))

(defn extract-chars
  "create a 2d array of all the characters and pipe to edn"
  [fpath]
  (txt->edn fpath
            (fn [lines]
              (->> lines
                   (mapv (fn[line](str/split line #"")))))))