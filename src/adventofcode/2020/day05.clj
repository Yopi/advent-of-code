(ns adventofcode.2020.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (str/split
   (slurp (clojure.java.io/file (clojure.java.io/resource  (apply str ["2020/day05/" file]))))
   #"\n"))

(defn parse-input [pass] (str/escape pass {\B 1 \F 0 \L 0 \R 1}))

(def testdata (parse-input "BFFFBBFRRR"))  ; row 70, column 7, seat ID 567.
(def testdata2 (parse-input "FFFBBBFRRR")) ; row 14, column 7, seat ID 119.
(def testdata3 (parse-input "BBFFBBFRLL")) ; row 102, column 4, seat ID 820.
(def inputdata (map parse-input (input "input.txt")))

(defn get-row [pass]
  (Integer/parseInt (subs pass 0 7) 2))

(defn get-seat [pass]
  (Integer/parseInt (subs pass 7) 2))

(defn generate-id [pass]
  (+ (* (get-row pass) 8)
        (get-seat pass)))

(def seat-ids (map generate-id inputdata))

(def max-seat-id
  (apply max seat-ids))

(def min-seat-id
  (apply min seat-ids))

(def part1 max-seat-id)

(def part2 (filter (complement (into #{} seat-ids)) (into #{} (range min-seat-id part1))))

(comment
  testdata
  (take-last (int (/ 8 2)) (range 8))
  (take-last (int (/ 4 2)) (range 4 8))
  (take-last (int (/ 2 2)) (range 6 8))
  (int (/ 127 2))
  (re-seq #"[BF]" "BFFFBBFRRR")
  (generate-id testdata3)
  (println (range min-seat-id part1))
  (complement (into #{} seat-ids))
  part1
  part2)
