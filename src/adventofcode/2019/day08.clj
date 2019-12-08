(ns adventofcode.2019.day08
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.pprint :as pp]))

(def inputdata
    (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day08/input.txt"))))
            #"")))
(def width 25)
(def height 6)

(def testdata [1,2,3,4,5,6,7,8,9,0,1,2])
(def testwidth 3)
(def testheight 2)

(def testdata2 [0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0])
(def testwidth2 2)
(def testheight2 2)


(defn divided-by-layer [input width height]
    (partition height (partition width input)))

(defn parse-data [layers]
    (for [layer layers]
        (let [numbers (flatten layer)]
            (hash-map
                :numbers numbers
                :zeroes (count (filter zero? numbers))
                :ones (count (filter #(not= 0 %) numbers)))
            )))

(defn count-ones-multiplied-by-twos [numbers]
    (let [ones (filter #(= 1 %) numbers)
        twos (filter #(= 2 %) numbers)]
        (* (count ones) (count twos))))

(def part1
    (as->(divided-by-layer inputdata width height) data
        (parse-data data)
        (apply min-key :zeroes data)
        (get data :numbers)
        (count-ones-multiplied-by-twos data)))

(defn get-color [pixels]
    (loop [pixel (first pixels) r (rest pixels)]
        (if (= 0 pixel)
            0
            (if (= 1 pixel)
                1
                (recur (first r) (rest r))))))
(def part2
    (divided-by-layer
        (let [layers (divided-by-layer inputdata width height)]
            (for [y (range height) x (range width)]
                (get-color (map #(nth % x) (map #(nth % y) layers)))))
         width height))
