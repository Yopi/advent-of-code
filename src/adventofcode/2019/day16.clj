(ns adventofcode.2019.day16
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn parse-input [file]
    (as-> (slurp (clojure.java.io/file (clojure.java.io/resource  file))) in
        (str/split in #"")
        (map #(Integer. %) in)))

(def input (parse-input "2019/day16/input.txt"))
(def input2 (apply concat (repeat 10000 input)))
(def test1 (map #(Integer. %) (str/split "12345678" #"")))
(def test2 (map #(Integer. %) (str/split "80871224585914546619083218645595" #"")))
(def test3 (map #(Integer. %) (str/split "19617804207202209144916044189917" #"")))
(def test4 (map #(Integer. %) (str/split "69317163492948606335995924319873" #"")))

(def phases (cycle '(1 0 -1 0)))

(defn get-pattern [position c]
    (drop 1 (take (+ c 1) (cycle 
        (flatten
            (concat 
                (take (+ position 1) (repeat 0))
                (take (+ position 1) (repeat 1))
                (take (+ position 1) (repeat 0))
                (take (+ position 1) (repeat -1))))))))

(defn calculate-fft [input, max-phases]
    (loop [phase 0
            in input]
        (do 
            (if (= phase max-phases)
                in
                (recur (+ phase 1) 
                    (for [i (range (count in))]
                        (let [adjustment (partition 2 (interleave in (get-pattern i (count in))))]
                            (mod (Math/abs (reduce + (map #(reduce * %) adjustment))) 10)
                    )))))))


(defn part1 []
    (apply concat (map str (take 8 (calculate-fft input 100)))))