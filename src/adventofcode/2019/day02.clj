(ns adventofcode.2019.day02
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def testdata [1 9 10 3 2 3 11 0 99 30 40 50])

(def inputdata
    (vec (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day02/input.txt"))))
            #","))))

(defn set-input [data x y]
    (assoc (assoc data 1 x) 2 y))

(def inputdata-part1 (set-input (vec inputdata) 12 2))

(defn value-at-position [data, pos]
    (nth data (nth data pos)))

(defn execute-computer [data]
    (loop [data data
            index 0]
        (let [opc (nth data index)]
            (case opc
                1 (recur (assoc data (nth data (+ index 3)) (+ (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                2 (recur (assoc data (nth data (+ index 3)) (* (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                99 data)
        )))

(def part1 (nth (execute-computer inputdata-part1) 0))

(def part2-solution
    (for [noun (range 100)
          verb (range 100)
          :let [compute (execute-computer (set-input inputdata noun verb))]
          :when (= (nth compute 0) 19690720)]
        [noun verb]))

(def part2
    (let [noun (first (flatten part2-solution))
          verb (second (flatten part2-solution))]
        (+
            (* 100 noun)
            verb)))
