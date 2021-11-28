(ns adventofcode.2019.day10
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.pprint :as pp]))

(def inputdata
    (map #(str/split % #"")
        (str/split
            (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day10/input.txt")))
            #"\n")))

(def testdata
    (map #(str/split % #"")
        (str/split
".#..#
.....
#####
....#
...##"
            #"\n")))

(def testdata2
    (map #(str/split % #"")
        (str/split
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"
            #"\n")))

(defn parse-map [data]
    (filter some?
        (for [y (range (count data)) x (range (count (first data)))]
            (if (= (nth (nth data y) x) "#")
                {:x x :y y :xy [x y]}
                nil))))

; Interleave two lists
(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn manhattan-dist [u v]
  (reduce +
    (map (fn [[a b]] (Math/abs (- a b))) (zip u v))))

(defn los [point data]
    (for [p data]
        (let [deg (Math/toDegrees (Math/atan2 (- (get p :y) (get point :y)) (- (get p :x) (get point :x))))
              dist (manhattan-dist [(get point :x) (get point :y)] [(get p :x) (get p :y)])]
              {deg dist})
        ))

(defn map-without-point [map point]
    (filter #(not= (:xy %) (get point :xy)) map))

(defn result-with-pos [result point]
    (filter #(= (:pos %) point) result))

(defn all-asteroids-seen [data]
    (let [map (parse-map data)]
        (for [point map]
            {
                :pos [(get point :x) (get point :y)]
                :los (count (into {} (los point (map-without-point map point))))
            }))
    )

(defn part1 [data]
    (apply max-key :los
        (all-asteroids-seen inputdata)))

(defn los-p2 [point data]
    (for [p data]
        (let [deg (+ (Math/toDegrees (Math/atan2 (- (get p :y) (get point :y)) (- (get p :x) (get point :x)))) 90)
              positive-deg (if (< deg 0) (+ deg 360) deg)
              dist (manhattan-dist [(get point :x) (get point :y)] [(get p :x) (get p :y)])]
              {:deg positive-deg :dist dist :pos (get p :xy)})
        ))

(defn part2 [data]
    (let [station-map (parse-map data)
        station {:x 11, :y 11, :xy [11 11]}]
        (let [stations (sort (group-by :deg (los-p2 station (map-without-point station-map (get station :xy)))))]
        (loop [station (last (first stations))
            rest-stations (rest stations)
            count 1]
            (if (= count 200)
                station
                (recur (last (first rest-stations)) (rest rest-stations) (+ count 1)))
            ))))
