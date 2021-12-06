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

(defn iterate [i days]
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
  (count (iterate in 80)))


(defn part2 [in]
  (count (iterate in 256)))
