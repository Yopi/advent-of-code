(ns adventofcode.2019.day03
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.data :as data]))

; Interleave two lists
(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn manhattan-dist [u v]
  (reduce +
    (map (fn [[a b]] (Math/abs (- a b))) (zip u v))))

(def testdata [["R75","D30","R83","U83","L12","D49","R71","U7","L72"], ["U62","R66","U55","R34","D71","R55","D58","R83"]])

(def inputdata
    (map #(str/split % #",")
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day03/input.txt")))
                #"\n")))

(defn generate-inbetween-nodes [[old-x old-y] [new-x new-y] distance]
    (if (= old-x new-x)
        (if (> new-y old-y)
            (zip (repeat old-x) (range old-y new-y))
            (zip (repeat old-x) (reverse (range new-y old-y))))
        (if (> new-x old-x)
            (zip (range old-x new-x) (repeat old-y))
            (zip (reverse (range new-x old-x)) (repeat old-y)))))

(defn generate-path [data]
    (apply concat
        (loop [data data
                nodes []
                x 0
                y 0]
            (if (empty? data)
                nodes
                (let [direction (first (first data))
                        distance (Integer. (str/join (rest (first data))))]
                    (let [new-pos (case direction
                                        \U [x (- y distance)]
                                        \R [(+ x distance) y]
                                        \D [x (+ y distance)]
                                        \L [(- x distance) y])]
                        (recur (rest data) (conj nodes (generate-inbetween-nodes [x y] new-pos distance)) (first new-pos) (second new-pos))))))))

(defn generate-paths [data]
    (map generate-path data))

(def testpaths
    (map generate-path testdata))

(def inputpaths
    (map generate-path inputdata))

(def part1
    (for [[x y] (let [[p1 p2] inputpaths]
                    (set/intersection (set p1) (set p2)))]
           {:x x :y y :dist (manhattan-dist [0 0] [x y])}))

(def part1-solution
    (apply min (map #(get % :dist) part1)))


(def part2
    (+ 1 (apply min (for [solution part1]
        (+
            (count (for [[x y] (first inputpaths)
                        :while (or (not= (get solution :x) x) (not= (get solution :y) y))]
                        [1]))
            (count (for [[x y] (second inputpaths)
                        :while (or (not= (get solution :x) x) (not= (get solution :y) y))]
                        [1])))))))


