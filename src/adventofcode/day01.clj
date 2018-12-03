(ns adventofcode.day01
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

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

; Based on https://stackoverflow.com/a/19896524/557715
(defn first-d [lst]
    (reduce
        (fn [acc [idx nxt]]
            (if (contains? acc nxt)
                (reduced nxt)
                (assoc acc nxt 1)))
        {} (map-indexed vector lst)))

(def part1
    (reduce + inputdata))

(def part2
    (first-d (reductions + (cycle inputdata))))
