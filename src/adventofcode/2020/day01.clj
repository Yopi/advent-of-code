(ns adventofcode.2020.day01
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.math.combinatorics :as combo]))

(def inputdata
    (map #(Integer. %)
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "2020/day01/input.txt")))
            #"\n")))

(defn possible-combinations [data x]
    (combo/combinations data x))

(defn get-2020 [combinations]
    (.indexOf (map #(apply + %) combinations) 2020))

(defn part1 [data]
    (apply *
        (let [combinations (possible-combinations data 2)]
            (nth combinations (get-2020 combinations)))))

(defn part2 [data]
    (apply *
        (let [combinations (possible-combinations data 3)]
            (nth combinations (get-2020 combinations)))))
