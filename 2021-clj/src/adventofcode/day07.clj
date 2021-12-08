(ns adventofcode.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [clojure.pprint :as pp]))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn parse-input [in]
  (as-> (apply str ["day07/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split f #",")
    (map #(Integer. %) f)))

; ((x y x y))
(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))

(defn part1 [i]
  (apply min (map (partial apply +) (map (fn [x] (map (fn [y] (abs (- x y))) i)) i))))

(defn summa [n]
  (/ (+ (* n n) n) 2))

(defn part2 [i]
  (apply min (map (partial apply +)
    (map (fn [x] (map (fn [y] (summa (abs (- x y)))) i)) (range (apply min i) (+ (apply max i)) )))))
