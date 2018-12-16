(ns adventofcode.day11
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def test-serial-1 18)
(def test-answer-1 '(33 45))
(def test-serial-2 42)
(def test-answer-1 '(21 61))

(def serial-nbr 8444)

;; Each fuel cell has a coordinate ranging from 1 to 300 in both the X (horizontal) and Y (vertical) direction.
;;   In X,Y notation, the top-left cell is 1,1, and the top-right cell is 300,1.

(def grid
    (for [y (range 1 301)
        x (range 1 301)]
         [x y]))


;; The power level in a given fuel cell can be found through the following process:
;;

;; Find the fuel cell's rack ID, which is its X coordinate plus 10.
(defn rack-id [cell]
    (+ (first cell) 10))

;; Begin with a power level of the rack ID times the Y coordinate.
(defn initial-power-level [cell]
    (* (rack-id cell) (second cell)))

;; Increase the power level by the value of the grid serial number (your puzzle input).
;; Set the power level to itself multiplied by the rack ID.
(defn inner-power-level [cell]
    (*
        (+ (initial-power-level cell) serial-nbr)
        (rack-id cell)))

;; Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
(defn keep-hundred [number]
    (Integer. (str (first (str (mod number 1000))))))

;; Subtract 5 from the power level.
(defn power-level [cell]
    (-
        (keep-hundred (inner-power-level cell))
        5))

(def power-levels (partition 300 (map power-level grid)))

(defn sum-square [x, y, size]
    (+
        (reduce + (take 3 (drop x (nth power-levels y))))
        (if (> size 0)
            (sum-square x (inc y) (dec size))
            0)))

(defn power-ranges [size]
    (for [y (range 0 (- 300 size))
        x (range 0 (- 300 size))]
         {:x (inc x), :y (inc y), :sum (sum-square x y size) :size size}))

(def part1
    (apply max-key :sum (power-ranges 3)))

;; Extremely slow
(def part2
    (apply max-key :sum (flatten (map power-ranges (range 3 300)))))
