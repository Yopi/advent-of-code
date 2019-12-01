(ns adventofcode.2018.day10
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(defn parse-positions [s]
    (map #(Integer. %) (rest (re-find #"position=<\s*([-\d]+),\s*([-\d]+)> velocity=<\s*([-\d]+),\s*([-\d]+)>" s))))

(def testdata
    (vec
        (map vec
        (map parse-positions
            (str/split
                (-> "day10/example.txt"
                    io/resource
                    io/file
                    slurp
                    str/trim)
                #"\n")))))

(def inputdata
    (vec
        (map vec
        (map parse-positions
            (str/split
                (-> "day10/input.txt"
                    io/resource
                    io/file
                    slurp
                    str/trim)
                #"\n")))))

(defn lazy-contains? [col key]
  (some #{key} col))

(defn update-position [[x y velx vely]]
    [(+ x velx) (+ y vely) velx vely])

(defn print-positions [mx]
    (let [min-x (apply min (map first mx))
            min-y (apply min (map second mx))
            max-x (inc (apply max (map first mx)))
            max-y (inc (apply max (map second mx)))]
        (do
            (let [rows (into {} (map (fn [[k v]] [k (map first v)]) (group-by second mx)))]
                (doseq [y (range min-y max-y)]
                    (do
                        (doseq [x (range min-x max-x)]
                            (if-let [row (get rows y)]
                                (if (lazy-contains? row x)
                                    (print "#")
                                    (print "."))
                                (print ".")))
                        (println ""))))
        mx)))

(defn square [a]
    (* a a))

(defn distance [a b]
    (Math/sqrt
        (+
            (square (- (first b) (first a)))
            (square (- (second b) (second a))))))

(defn avg-dist [p lst]
    (/
        (reduce + (map #(distance p %) lst))
        (count lst)))

;; To be able to figure out if we are at a minima
(defn average-distance [lst]
    (/
        (reduce +
            (for [l lst]
                (avg-dist l lst)))
        (count lst)))


(defn update-positions [matrix]
    (loop [avg-dist 99999 time-taken 0 mx matrix]
        (let [new-mx (map update-position mx)
                new-avg (average-distance new-mx)]
            (if (> new-avg avg-dist)
                [print-positions mx)
                    time-taken]
                (recur new-avg (inc time-taken) new-mx))))

