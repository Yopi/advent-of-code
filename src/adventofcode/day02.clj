(ns adventofcode.day02
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.data :as data]))

(def inputdata
    (str/split
        (slurp (io/file (io/resource  "day02/input.txt")))
        #"\n"))

(def testdata
    (str/split
        (slurp (io/file (io/resource  "day02/example.txt")))
        #"\n"))

(def testdata2
    (str/split
        (slurp (io/file (io/resource  "day02/example2.txt")))
        #"\n"))

(def get-occurences
    (map set (map vals (map frequencies (map seq inputdata)))))

(def part1
    (*
        (count (remove nil? (map #(some #{3} %) get-occurences)))
        (count (remove nil? (map #(some #{2} %) get-occurences)))))

(def part2
    (apply str (first
        (filter #(= (count %) 25) ; Get off-by-one
            (map  #(remove nil? %) ; So we can remove elements we failed to match
                (mapcat identity ; Flatten one layer
                    (for [x inputdata]
                        (filter #(vector? %) ; Matching with itself
                            (remove nil? ; No matches
                                (map #(get % 2) (map #(data/diff (seq x) %) (map seq inputdata))))))))))))
