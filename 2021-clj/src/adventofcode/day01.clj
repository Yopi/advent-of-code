(ns adventofcode.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def inputdata
  (map #(Integer. %)
       (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "day01/input")))
        #"\n")))

(def testdata
  (map #(Integer. %)
       (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "day01/testdata")))
        #"\n")))

(defn count-increased [in]
  (count (filter true? (map #(> (second %) (first %)) (partition 2 1 in)))))

(def part1 (count-increased inputdata))

(def part2 (count-increased (map #(apply + %) (partition 3 1 inputdata))))
