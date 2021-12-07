(ns adventofcode.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn parse-input [in]
  (as-> (apply str ["day06/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split f #",")
    (map #(Integer. %) f)))

; ((x y x y))
(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))

(defn iter [i days]
  (loop [day    1
         fishes i]
    (let [iterated-fishes    (map dec fishes)
          fishes-under-0     (filter #(< % 0) iterated-fishes)
          fishes-over-0      (filter #(>= % 0) iterated-fishes)
          replacement-fishes (flatten (map (fn [_xs] [6 8]) fishes-under-0))
          new-fishes         (concat fishes-over-0 replacement-fishes)]
      (if (= day days)
        new-fishes
        (recur (inc day) new-fishes)))))

(defn part1 [in]
  (iter in 80))

(defn convert-input [i]
  (vec
    (let [v (frequencies i)]
      (for [i (range 0 9)]
        (get v i 0)))))

(defn rotate [i]
  (take (count i) (drop 1 (cycle i))))

(defn iter-part2 [i days]
  (loop [day    1
         fishes i]
    (let [rotated-fishes (vec (rotate fishes))
          born-fishes (nth rotated-fishes 8)
          rotated-fishes-with-born (update rotated-fishes 6 (fn [x] (+ x born-fishes)))]
      (if (= day days)
        rotated-fishes-with-born
        (recur (inc day) rotated-fishes-with-born)))))

(defn part2 [in]
  (apply + (iter-part2 (convert-input in) 256)))
