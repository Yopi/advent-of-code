(ns adventofcode.day22
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day22/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn parse-input [in]
  (as-> in i
    (re-find #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" i)
    {:on (= (nth i 1) "on")
     :x1  (Integer. (nth i 2))
     :x2  (inc (Integer. (nth i 3)))
     :y1  (Integer. (nth i 4))
     :y2  (inc (Integer. (nth i 5)))
     :z1  (Integer. (nth i 6))
     :z2  (inc (Integer. (nth i 7)))}))

(def inputdata (map parse-input (read-input "input")))
(def testdata (map parse-input (read-input "testdata")))
(def testdata2 (map parse-input (read-input "testdata2")))
(def testdata3 (map parse-input (read-input "testdata3")))

(defn out-of-bounds? [step mn mx]
  (or
   (and (< (step :x1) mn) (< (step :x2) mn))
   (and (< (step :y1) mn) (< (step :y2) mn))
   (and (< (step :z1) mn) (< (step :z2) mn))
   (and (> (step :x1) mx) (> (step :x2) mx))
   (and (> (step :y1) mx) (> (step :y2) mx))
   (and (> (step :z1) mx) (> (step :z2) mx))))

(defn get-coords-ranges [mn mx step]
  (if (out-of-bounds? step mn mx)
    #{}
    (let [power (if (step :on) 1 0)]
      [power (into #{}
                   (for [x (range (step :x1) (step :x2))
                         y (range (step :y1) (step :y2))
                         z (range (step :z1) (step :z2))]
                     [x y z]))])))

(defn part1 [in]
  (loop [step (first in)
         steps (rest in)
         powered-on #{}]
    (if (nil? step)
      powered-on
      (if (step :on)
        (recur (first steps) (rest steps) (set/union powered-on (second (get-coords-ranges -50 50 step))))
        (recur (first steps) (rest steps) (set/difference powered-on (second (get-coords-ranges -50 50 step))))))))

(defn contains-cube? [c1 c2]
  (and
   (<= (c1 :x1) (c2 :x1))
   (>= (c1 :x2) (c2 :x2))
   (<= (c1 :y1) (c2 :y1))
   (>= (c1 :y2) (c2 :y2))
   (<= (c1 :z1) (c2 :z1))
   (>= (c1 :z2) (c2 :z2))))

(defn cube-volume [[x1 x2] [y1 y2] [z1 z2]]
  (*
   (- x2 x1)
   (- y2 y1)
   (- z2 z1)))

(defn points [& ps]
  (vec (sort (set ps))))

; Remove c2 from c1
(defn subtract [c1 c2]
  (if (or
       (<= (c1 :x2) (c2 :x1))
       (<= (c2 :x2) (c1 :x1))
       (<= (c1 :y2) (c2 :y1))
       (<= (c2 :y2) (c1 :y1))
       (<= (c1 :z2) (c2 :z1))
       (<= (c2 :z2) (c1 :z1)))
    [c1]
    (let [xs (points (c1 :x1) (c1 :x2) (c2 :x1) (c2 :x2))
          ys (points (c1 :y1) (c1 :y2) (c2 :y1) (c2 :y2))
          zs (points (c1 :z1) (c1 :z2) (c2 :z1) (c2 :z2))]
      (for [x (partition 2 1 xs)
            y (partition 2 1 ys)
            z (partition 2 1 zs)
            :let [cube {:x1 (first x) :x2 (second x)
                        :y1 (first y) :y2 (second y)
                        :z1 (first z) :z2 (second z)
                        :on (c1 :on)}]
            :when (contains-cube? c1 cube)
            :when (not (contains-cube? c2 cube))]
        cube))))

(defn cube-count [cubes]
  (map (fn [c]
         (if (c :on)
           (cube-volume [(c :x1) (c :x2)] [(c :y1) (c :y2)] [(c :z1) (c :z2)])
           0)) cubes))

(defn cube-division [in]
  (loop [step (first in)
         steps (rest in)
         cubes []
         i 0]
    (if (nil? step)
      cubes
      (let [new-steps (mapcat (fn [cube] (subtract cube step)) steps)]
        (recur (first new-steps) (vec (rest new-steps)) (conj cubes step) (inc i))))))

(defn part2 [in]
  (reduce + (cube-count (cube-division (reverse in)))))
