(ns adventofcode.day06
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.data :as data]))

; Data in format "x, y"
;(def inputdata
;    (map #(map read-string %)
;        (vec (map #(str/split % #", ")
;            (str/split
;                (-> "day06/example.txt"
;                    io/resource
;                    io/file
;                    slurp
;                    str/trim) #"\n")))))

(def inputdata
    (map #(map read-string %)
        (vec (map #(str/split % #", ")
            (str/split
                (-> "day06/input.txt"
                    io/resource
                    io/file
                    slurp
                    str/trim) #"\n")))))


(def width
    (reduce max (map first inputdata)))

(def height
    (reduce max (map second inputdata)))

; Interleave two lists
(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn manhattan-dist [u v]
  (reduce +
    (map (fn [[a b]] (Math/abs (- a b))) (zip u v))))

;; Returns (distance coordinate origin)
(defn compare-and-keep [u v]
    [(manhattan-dist u v) v u])

; List of all (x y) coordinates we are considering
(def coordinates
    (for [x (range 0 (inc width))
         y (range 0 (inc height))]
         [x y]))

(defn boundary-coordinate [xy]
    (if (or (= (first xy) 0)
            (= (second xy) 0)
            (= (first xy) width)
            (= (second xy) height)) true false))

(def infinite-coordinates
    (filter #(boundary-coordinate %) coordinates))

(defn group-data [mp]
    (group-by second
        (mapcat identity (for [xy mp] (map #(compare-and-keep xy %) coordinates)))))

(defn more-than-one? [lst]
    (if (> (count lst) 1) true false))

(defn clean-data [mp]
    (reduce-kv
        (fn [m k v]
            (let [mv (apply min (map first v))
                  filtered-vals (filter #(= mv (first %)) v)]
                (if (more-than-one? filtered-vals)
                    m
                    (assoc m k (nth (first filtered-vals) 2)))
                )) {} (group-data mp)))

(defn lazy-contains? [col key]
  (some #{key} col))

(def clean-inputdata
    (clean-data inputdata))

; All coords that have an infinite area
(def coordinates-with-infinite-area
    (distinct
        (remove nil?
            (map #(if (lazy-contains? infinite-coordinates (first %)) (second %) nil) clean-inputdata))))

(defn pruned-coordinate? [coord]
    (if (lazy-contains? coordinates-with-infinite-area coord) true false))

(def part1
    (apply max-key val
        (frequencies (remove pruned-coordinate? (vals clean-inputdata)))))

(defn under-limit? [dist]
    (> 32 dist))

(def part2
    (count (filter under-limit?
        (for [xy coordinates]
            (reduce + (map #(manhattan-dist xy %) inputdata))))))








