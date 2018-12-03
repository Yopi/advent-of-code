(ns adventofcode.day03
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.matrix :as matrix]
    ))

(def inputdata
    (map #(str/split % #": ")
        (str/split
            (str/replace
                (str/replace
                    (slurp (io/file (io/resource  "day03/input.txt")))
                    #"\#\d+ @ " "")
                #"x" ",")
            #"\n")))

(def testdata
    (map #(str/split % #": ")
        (str/split
            (str/replace
                (str/replace
                    (slurp (io/file (io/resource  "day03/example.txt")))
                    #"\#\d+ @ " "")
                #"x" ",")
            #"\n")))

(def matrix-width 1000)
(def matrix-height 1000)

(defn offset-size-to-matrix [[offset size]]
    (let [s-offset (str/split offset #",")
          s-size (str/split size #",")
          mx (matrix (to-array-2d (repeat matrix-height (repeat matrix-width 0))))]
     (doseq [r (range (Integer. (first s-offset)) (+ (Integer. (first s-offset)) (Integer. (first s-size))))
           w (range (Integer. (second s-offset)) (+ (Integer. (second s-offset)) (Integer. (second s-size))))]
           (set-selection! mx r w 1))
    mx))

(def input-to-matrix
    (reduce + (map offset-size-to-matrix inputdata)))

(def test-to-matrix
    (reduce + (map offset-size-to-matrix testdata)))

;; Part 1
(defn input-to-number [mx]
    (count (filter #(> % 1) (eseq mx))))

;; Part 2
(defn find-non-overlapping-id [i]
    ; For every elf, multiply with elf matrix and see if there is anything but ones
    (if (= (count (filter #(> % 1) (eseq (* mx (offset-size-to-matrix (nth inputdata i)))))) 0)
        ; If not, then we have found the correct
        (+ i 1)
        ; Otherwise try the next elf
        (recur (inc i))))
