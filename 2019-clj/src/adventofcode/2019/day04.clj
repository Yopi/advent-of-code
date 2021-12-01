(ns adventofcode.2019.day04
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def inputdata [347312 805915])

(def inputdata-range (range (first inputdata) (second inputdata)))

(defn consecutive-criteria [n]
    (some?
        (re-find #"(\d)\1" (str n))))

(defn increasing-criteria [n]
    (every?
        #(<= % 0)
        (map #(apply - %) (partition 2 1 (map #(Integer. %) (str/split (str n) #""))))))

(defn p1-meets-criteria [n]
    (and (consecutive-criteria n) (increasing-criteria n)))

(defn consecutive-unique-criteria [n]
    (some?
        (re-find #"(^|[1-9])00([1-9]|$)|(^|[0,2-9])11([0,2-9]|$)|(^|[0-1,3-9])22([0-1,3-9]|$)|(^|[0-2,4-9])33([0-2,4-9]|$)|(^|[0-3,5-9])44([0-3,5-9]|$)|(^|[0-4,6-9])55([0-4,6-9]|$)|(^|[0-5,7-9])66([0-5,7-9]|$)|(^|[0-6,8-9])77([0-6,8-9]|$)|(^|[0-7,9])88([0-7,9]|$)|(^|[0-8])99([0-8]|$)" (str n))))

(defn p2-meets-criteria [n]
    (and (consecutive-unique-criteria n) (increasing-criteria n)))

(def part1
    (reduce +
        (for [n inputdata-range]
            (if (p1-meets-criteria n)
                1
                0))))

(def part2
    (reduce +
        (for [n inputdata-range]
            (if (p2-meets-criteria n)
                1
                0))))

