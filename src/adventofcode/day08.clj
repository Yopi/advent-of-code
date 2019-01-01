(ns adventofcode.day08
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def testdata
    (map #(Integer. %)
        (str/split
            (-> "day08/example.txt"
                io/resource
                io/file
                slurp
                str/trim)
            #" ")))

(def inputdata
    (map #(Integer. %)
        (str/split
            (-> "day08/input.txt"
                io/resource
                io/file
                slurp
                str/trim)
            #" ")))

(defn parse-header [input]
    [(take 2 input) (drop 2 input)])

(defn num-children [input]
    (first input))

(defn num-metadata [input]
    (second input))

;;; parse
;; parse-header
; [2 3] [0 3 10 11 12 1 1 0 1 99 2 1 1 2]
;; parse-header
; [0 3] [10 11 12 1 1 0 1 99 2 1 1 2]
;; parse-body
; [0] [3] [10 11 12] | [1 1 0 1 99 2 1 1 2]

;; Recursiveness inspired by https://www.reddit.com/r/adventofcode/comments/a47ubw/2018_day_8_solutions/ebcpzxg/
(defn parse [input]
    (let [[header remaining] (parse-header input)]
        (loop [cs []
                input remaining
                num-childs (num-children header)]
            (if (= num-childs 0)
                [{:nodes cs :metadata (conj (take (num-metadata header) input))}
                (drop (num-metadata header) input)]
                (let [[child input] (parse input)]
                    (recur (conj cs child)
                            input
                            (dec num-childs)))))))

(def part1-test
    (first (parse testdata)))

(def part1
    (first (parse inputdata)))

(defn sum-metadata [node]
    (+ (reduce + (get node :metadata))
        (reduce + (map sum-metadata (get node :nodes)))))

(defn get-value [node]
    (if (empty? (get node :nodes))
        (reduce + (get node :metadata))
        (->> (get node :metadata)
                (filter #(>= (count (get node :nodes)) %))
                (map dec)
                (map #(nth (get node :nodes) %))
                (map get-value)
                (reduce +)
            )))
