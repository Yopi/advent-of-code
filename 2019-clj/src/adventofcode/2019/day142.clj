(ns adventofcode.2019.day142
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn to-chem-and-quantity [s]
    (let [[quantity chemical] (str/split s #" ")]
        {chemical (Integer. quantity)}))

(defn to-reaction [r]
    (let [lh (str/split (first r) #", ")
            rh (str/split (second r) #", ")]
        [(into {} (map to-chem-and-quantity lh)) (into {} (map to-chem-and-quantity rh))]
        ))

(defn parse-input [file]
    (as-> (slurp (clojure.java.io/file (clojure.java.io/resource  file))) in
        (str/split in #"\n")
        (map #(str/split % #" => ") in)
        (map to-reaction in)))

(def test1 (parse-input "2019/day14/test1.txt"))
(def test2 (parse-input "2019/day14/test2.txt"))
(def test3 (parse-input "2019/day14/test3.txt"))
(def test4 (parse-input "2019/day14/test4.txt"))
(def test5 (parse-input "2019/day14/test5.txt"))
(def input (parse-input "2019/day14/input.txt"))

;; Actual impl

(defn resources-required-for [reactions want-to-get quantity]
    (map (fn [[key value]] [key (if (< (/ value quantity) (* 2 value)])
        (first
            (filter #(contains? (second %) want-to-get) reactions)))

(defn x [])
