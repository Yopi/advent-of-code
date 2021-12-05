(ns adventofcode.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn split-coords [coords]
  (map #(Integer. %)
    (re-seq #"\d+" coords)))

(defn parse-input [in]
  (as-> (apply str ["day05/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split-lines f)
    (map split-coords f)))

; ((x y x y))
(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))

(defn min-max-v [i fnc]
  (let [xs (flatten (map fnc i))]
    (vector (apply min xs)
      (apply max xs))))

(defn min-max-x [i]
  (min-max-v i #(vector (nth % 0) (nth % 2))))

(defn min-max-y [i]
  (min-max-v i #(vector (nth % 1) (nth % 3))))

(defn get-line [i]
  (let [[x1 y1 x2 y2] i
        min-x         (min x1 x2)
        max-x         (max x1 x2)
        min-y         (min y1 y2)
        max-y         (max y1 y2)]
    ; Only straight lines
    (cond
      (and (not= x1 x2) (not= y1 y2)) []
      (not= x1 x2) (for [x (range min-x (inc max-x))]
                     [[x y1] 1])
      (not= y1 y2) (for [y (range min-y (inc max-y))]
                     [[x1 y] 1]))))

(defn part1 [i]
  (let [lines        (map get-line i)
        lines-dict   (map #(into {} %) lines)
        summed-lines (apply (partial merge-with +) lines-dict)]
    (count (filter #(> % 1) (vals summed-lines)))))

(defn get-line-diag [i]
  (let [[x1 y1 x2 y2] i
        min-x         (min x1 x2)
        max-x         (max x1 x2)
        min-y         (min y1 y2)
        max-y         (max y1 y2)
        flipped-x     (< x2 x1)
        flipped-y     (< y2 y1)]

    (if
      (and (not= x1 x2) (not= y1 y2)) ; Diagonal line
      (if (= flipped-x flipped-y) ; If y goes from low to high or vice versa
        (for [x    (range min-x (inc max-x))
              :let [y (+ min-y (- x min-x))]]
          [[x y] 1])
        (for [x    (range min-x (inc max-x))
              :let [y (- max-y (- x min-x))]]
          [[x y] 1]))
      (if (not= x1 x2)
        (for [x (range min-x (inc max-x))]
          [[x y1] 1])
        (for [y (range min-y (inc max-y))]
          [[x1 y] 1])))))

; Debugging test input
(defn print-map [i]
  (for [x (range 0 10)]
    (for [y (range 0 10)]
      (if (get i [y x])
        (str (get i [y x]))
        "."))))

(defn part2 [i]
  (let [lines        (map get-line-diag i)
        lines-dict   (map #(into {} %) lines)
        summed-lines (apply (partial merge-with +) lines-dict)]
    (count (filter #(> % 1) (vals summed-lines)))))
