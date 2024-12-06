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