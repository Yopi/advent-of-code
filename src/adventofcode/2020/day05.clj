(ns adventofcode.2020.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (str/split
   (slurp (clojure.java.io/file (clojure.java.io/resource  (apply str ["2020/day05/" file]))))
   #"\n"))

(defn parse-input [pass]
  [(re-seq #"[BF]" pass)
   (re-seq #"[LR]" pass)])

(def testdata (parse-input "BFFFBBFRRR"))  ; row 70, column 7, seat ID 567.
(def testdata2 (parse-input "FFFBBBFRRR")) ; row 14, column 7, seat ID 119.
(def testdata3 (parse-input "BBFFBBFRLL")) ; row 102, column 4, seat ID 820.
(def inputdata (map parse-input (input "input.txt")))

(defn find-seat [pass seats]
  (loop [n (first pass)
         acc  (rest pass)
         seat seats]
    (if (empty? n)
      (first seat)
      (if (or (= n "F") (= n "L"))
        (recur (first acc) (rest acc) (take (/ (count seat) 2) seat))
        (recur (first acc) (rest acc) (take-last (int (/ (count seat) 2)) seat))))))

(defn generate-id [pass]
  (+ (* (find-seat (first pass) (range 128)) 8)
     (find-seat (second pass) (range 8))))

(def seat-ids (map generate-id inputdata))

(def part1
  (apply max seat-ids))

(def min-seat-id
  (apply min seat-ids))

(def part2 (filter (complement (into #{} seat-ids)) (into #{} (range min-seat-id part1))))

(comment
  testdata
  (find-seat (second testdata3) (range 8))
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
