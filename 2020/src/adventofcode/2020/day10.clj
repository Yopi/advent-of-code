(ns adventofcode.2020.day10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn input [file]
  (as-> (apply str ["2020/day10/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(Long. %) f)
    (vec f)
    (sort f)))

(def testdata (input "test1.txt"))
(def testdata2 (input "test2.txt"))
(def inputdata (input "input.txt"))

(defn joltage-diff [joltage adapter]
  (- adapter joltage))

(defn valid-joltage-diff [joltage adapter]
  (<= (joltage-diff joltage adapter) 3))

(defn count-voltages [adapters current]
  (loop [adapter (first adapters)
         rest-of-adapters (rest adapters)
         current-joltage current
         joltage-differences []]
    (if (nil? adapter)
      joltage-differences
      (let [new-diff (joltage-diff current-joltage adapter)]
        (if (or (or (= new-diff 1) (= new-diff 2)) (= new-diff 3))
          (recur (first rest-of-adapters) (rest rest-of-adapters) adapter (conj joltage-differences new-diff))
          (recur (first rest-of-adapters) (rest rest-of-adapters) current-joltage joltage-differences))))))

(defn part1 [adapters]
  (let [diffs (frequencies (conj (count-voltages adapters 0) 3))]
    (* (get diffs 1) (get diffs 3))))

(defn part2 [adapters]
  (let [partitions (filter #(some #{1} %) (partition-by identity (count-voltages adapters 0)))
        counter (fn [x] (as-> (combo/partitions x :min 1 :max 3) parts
                          (map combo/permutations parts)
                          (map count parts)
                          (reduce + parts)
                          ))]
    (reduce * (map counter partitions))))

(comment
  (part1 inputdata)
  (part2 inputdata))
