(ns adventofcode.2019.day01
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(def inputdata
    (map #(Integer. %)
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day01/input.txt")))
            #"\n")))

(defn calculate-mass [in]
    (max ; Handle negative fuel
        (- (quot in 3) 2)
        0))

(defn calculate-fuel [in]
    (loop [cnt in
           acc 0]
           (if (zero? cnt)
            acc
            (let [fuel-for-fuel (calculate-mass cnt)]
            (recur fuel-for-fuel (+ acc fuel-for-fuel))))))

(def part1
    (reduce + (map calculate-mass inputdata)))

(def part2
    (reduce + (map calculate-fuel inputdata)))
