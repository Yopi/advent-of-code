(ns adventofcode.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.pprint :as pp]))

(defn parse-input [in]
  (as-> (apply str ["day08/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)
    (map (fn [x] (str/split x #" \| ")) f)
    (map (fn [x]
           {:unique (str/split (first x) #" ")
            :output (str/split (second x) #" ")}) f)))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn part1 [in]
  (count (flatten
           (map (fn [x] (filter (fn [y] (some #{2 3 4 7} #{y})) x))
             (map (fn [x] (map count (get x :output))) in)))))

(defn create-mapping [in]
  (let [unique       (get in :unique)
        one          (filter #(= 2 (count %)) unique)
        seven        (filter #(= 3 (count %)) unique)
        four         (filter #(= 4 (count %)) unique)
        eight        (filter #(= 7 (count %)) unique)

        twothreefive (filter #(= 5 (count %)) unique)
        zerosixnine  (filter #(= 6 (count %)) unique)

        three        (filter #(every? (set %) (set (first seven))) twothreefive)
        nine         (filter #(every? (set %) (set (first three))) zerosixnine)
        six          (filter #(= (count (set/intersection (set %) (set (first one)))) 1) zerosixnine)
        zero         (filter #(and (not= (first six) %) (not= (first nine) %)) zerosixnine)
        five         (filter #(every? (set (first six)) (set %)) twothreefive)
        two          (filter #(and (not= (first three) %) (not= (first five) %)) twothreefive)]
    {(sort (first zero))  0
     (sort (first one))   1
     (sort (first two))   2
     (sort (first three)) 3
     (sort (first four))  4
     (sort (first five))  5
     (sort (first six))   6
     (sort (first seven)) 7
     (sort (first eight)) 8
     (sort (first nine))  9}))

(defn output [in]
  (let [mapping (create-mapping in)
        output  (map sort (get in :output))]
    (map (fn [x] (get mapping x)) output)))

(defn part2 [in]
  (apply +
    (map #(Integer. %)
      (map str/join
        (map output in)))))
