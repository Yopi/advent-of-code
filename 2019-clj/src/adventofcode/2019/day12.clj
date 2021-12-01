(ns adventofcode.2019.day12
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.pprint :as pp]
        [clojure.math.numeric-tower :as math]))

(defn print-coordinates [data]
    (println
        (for [coordinate data]
            (str "pos=<x=" (nth coordinate 0) ", y=" (nth coordinate 1) ", z=" (nth coordinate 2)))))

(defn parse-coordinate [coordinate]
    (let [pos (str/split coordinate #"=")]
        (Integer. (second pos))))

(defn parse-coordinates [coordinates]
    (partition 3 
            (map parse-coordinate 
                (map first 
                    (re-seq #"([xyz]=-?\d+)" 
                        coordinates)))))

(def inputdata
    (parse-coordinates 
        (slurp 
            (clojure.java.io/file (clojure.java.io/resource  "2019/day12/input.txt")))))

(def testdata
    (parse-coordinates 
        (slurp 
            (clojure.java.io/file (clojure.java.io/resource  "2019/day12/testdata.txt")))))

(defn transpose [& xs]
    (apply map list xs))

(defn compare-coordinate [x1 x2]
    (if (> x1 x2)
        -1
        (if (= x1 x2)
            0
            1))) 

(defn difference-coordinates [c1 c2]
    (for [i (range (count c1))
            :let [m1 (nth c1 i)
                    m2 (nth c2 i)]]
        (map #(reduce - %) (transpose m1 m2)))
)
                 
(defn calc-velocity [velocity m1 m2]
    (-> (vec velocity)
        (assoc 0 (compare-coordinate (nth m1 0) (nth m2 0)))
        (assoc 1 (compare-coordinate (nth m1 1) (nth m2 1)))
        (assoc 2 (compare-coordinate (nth m1 2) (nth m2 2)))))

(defn calc-velocities [data velocities]
    (for [i (range (count data))
            :let [moon (nth data i)
                    velocity (nth velocities i)]]
            (map #(reduce + %) (transpose velocity (map #(reduce + %)
                (apply transpose 
                    (for [other-moons data]
                        (calc-velocity velocity moon other-moons)
        )))))))

(defn init-velocities []
    [
        '(0, 0, 0)
        '(0, 0, 0)
        '(0, 0, 0)
        '(0, 0, 0)
    ])

(defn apply-velocity [coordinates velocities]
    (doall (for [cv (partition 2 (interleave coordinates velocities))]
        (doall (map #(reduce + %) (apply transpose cv))))))



(defn simulate-gravity [data total]
    (loop [coordinates data
            previous-coordinates data
            velocities (init-velocities)
            steps 0]
            (if (= steps total)
                [steps coordinates velocities]
                (let [new-velocities (calc-velocities coordinates velocities)]
                    (if (= '(0 0 0 0) (map first velocities))
                        (println "x: " steps)
                        0)
                    (if (= '(0 0 0 0) (map second velocities))
                        (println "y: " steps)
                        0)
                    (if (= '(0 0 0 0) (map last velocities))
                        (println "z: " steps)
                        0)
                    (if (> steps 280000)
                        0
                    (recur
                       (apply-velocity coordinates new-velocities)
                       coordinates
                       new-velocities
                       (+ 1 steps))
                    )
            ))))

(defn calc-total-energy [coordinates velocities]
    (reduce + 
        (for [i (range (count coordinates))
            :let [moon (nth coordinates i)
                    velocity (nth velocities i)]]
        (* 
            (reduce + (map #(Math/abs %) moon))
            (reduce + (map #(Math/abs %) velocity)))
    )))

(defn part1 [] 
    (let [out (simulate-gravity inputdata 1000)
            coordinates (nth out 1)
            velocities (nth out 2)]
            (calc-total-energy coordinates velocities)))


(defn part2 [] 
    (let [x 167624
            y 231614
            z 102356]
        (math/lcm (math/lcm x y) z)))

