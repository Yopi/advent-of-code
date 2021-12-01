(ns adventofcode.2020.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn input [file]
  (as-> (apply str ["2020/day06/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n")
    ;(map #(str/replace % #"\n" "") f)
    (map #(str/split % #"\n") f)
    (for [i f]
      (map #(into #{} %)
           (map #(str/split % #"") i))
      )))

(def testdata (input "test1.txt"))

(def inputdata (input "input.txt"))

(defn part1 [in]
  (reduce + (map count
                 (for [form in]
                   (apply set/union form)))))

(part1 inputdata)

(defn part2 [in]
  (reduce + (map count
                  (for [form in]
                    (apply set/intersection form)))))

(part2 inputdata)

(comment)
