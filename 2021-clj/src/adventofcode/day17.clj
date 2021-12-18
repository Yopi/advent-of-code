(ns adventofcode.day17
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [ubergraph.core :as uber]
   [ubergraph.alg :as ualg]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day17/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)))

(defn parse-input [in]
  (let [[_, x1, x2, y1, y2] (re-find #"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)" in)]
    [
    (Integer. x1)
    (Integer. x2)
    (Integer. y1)
    (Integer. y2)
    ]))

(defn toward-zero [x]
  (if (< x 0)
    (inc x)
    (if (> x 0)
      (dec x)
      x)))

(defn inside-range? [x y in]
  (if (and
        (and
          (>= x (get in 0))
          (<= x (get in 1)))
        (and
          (>= y (get in 2))
          (<= y (get in 3))))
      true
      false))

(defn step [in initial-probe]
  (loop [probe initial-probe
         max-height -1]
    (let [{:keys [x y xvel yvel]} probe]
      (cond
        (inside-range? x y in) max-height
        (> x (get in 1)) -1
        (< y (get in 2)) -2
        :else (recur
          {:x (+ x xvel)
            :y (+ y yvel)
            :xvel (toward-zero xvel)
            :yvel (- yvel 1)}
          (if (> y max-height) y max-height))
      ))))

(def inputdata (parse-input "target area: x=85..145, y=-163..-108"))
(def testdata (parse-input "target area: x=20..30, y=-10..-5"))

(defn part1 [in]
  (apply max
    (for [xs (range 1 (inc (get in 1)))
          ys (range 1 (* 2 (inc (get in 1))))]
          (step in {:x 0 :y 0 :xvel xs :yvel ys}))))

(defn part2 [in]
  (count (distinct (filter #(not (empty? %))
    (for [xs (range 0 (inc (max (get in 0) (get in 1))))
          ys (range (dec (get in 2)) (- (* 2 (get in 3))))]
          (let [height (step in {:x 0 :y 0 :xvel xs :yvel ys})]
            (if (>= height 0)
              [xs ys]
              [])))))))
