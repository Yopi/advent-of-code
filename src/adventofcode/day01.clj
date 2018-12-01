(ns adventofcode.day01
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))


(def inputdata
    (map #(Integer. %)
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "day01/input1")))
            #"\n")))

(def testdata2
    (map #(Integer. %)
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "day01/test2")))
            #"\n")))


(def part1
    (reduce + (map #(Integer. %) inputdata)))

(defn part2 [c]
    (let [found (take-n-duplicates c)]
        (if (> (count found) 1)
            (first (first found)); Get the value from the frequency map
            (recur (inc c))))); Iterate

; No easy way to take from a lazy list
(defn take-n-duplicates [c]
    (filter #(> (second %) 1) (frequencies (take c (reductions + (map #(Integer. %) (cycle inputdata)))))))
