(ns adventofcode.2020.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn input [file]
  (as-> (apply str ["2020/day09/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(Long. %) f)
    (vec f)))

(def testdata (input "test1.txt"))
(def inputdata (input "input.txt"))

(defn possible-combinations [data x]
  (combo/combinations data x))

(defn lazy-contains? [col key]
  (if (nil? (some #{key} col))
    false
    true
    ))

(defn preamble-contains-sum? [rolling-previous preamble num]
  (lazy-contains? (map #(reduce + %) (possible-combinations rolling-previous 2)) num))

(defn validate [preamble data]
  (loop [current (first (drop preamble data))
         in (rest (drop preamble data))
         rolling-previous (reverse (take preamble data))
         index 0]
    (if (nil? current)
      index
      (if (preamble-contains-sum? rolling-previous preamble current)
        (recur (first in) (rest in) (conj (drop-last 1 rolling-previous) current) (inc index))
        current        
        ))))
  
(defn part1 []
  (validate 25 inputdata))

(def p1-test 127)
(def p1 41682220)

(defn find-sum-range [data to-find]
  (loop [index 0
         len 0]
    (let [sum-of-range (reduce + (take len (drop index data)))]
      (if (= sum-of-range to-find)
        (take len (drop index data))
        (if (or (= len (count data)) (> sum-of-range to-find))
          (recur (inc index) 0)
          (recur index (inc len)))))))

(defn part2 [data number-to-find]
  (let [r (find-sum-range data number-to-find)
        sorted (sort r)
        smallest (first sorted)
        largest (last sorted)]
    (+ smallest largest)))

(comment
  (map #(reduce + %) (possible-combinations (take 5 testdata) 2))
  (preamble-contains-sum? (take 5 testdata) 5 40)
  (validate 5 testdata)
  (validate 25 inputdata)
  (find-sum-range inputdata p1)
  (part2 testdata p1-test)
  (part2 inputdata p1)
  )
