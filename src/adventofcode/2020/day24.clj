(ns adventofcode.2020.day24
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-line [l]
  (map first (re-seq #"(se|sw|ne|nw|e|w)" l)))

(defn input [file]
  (as-> (apply str ["2020/day24/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map parse-line f)
    ))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(def adjacent-tile (parse-line "esew"))
(def self-tile (parse-line "nwwswee"))

(defn parse-coordinate [in]
  (loop [coord (first in)
         coords (rest in)
         [x y] [0 0]]
    (if (nil? coord)
      [x y]
      (recur
       (first coords)
       (rest coords)
       (case coord
         "w"  [(- x 2) y]
         "nw" [(- x 1) (- y 1)]
         "sw" [(- x 1) (+ y 1)]
         "e"  [(+ x 2) y]
         "ne" [(+ x 1) (- y 1)]
         "se" [(+ x 1) (+ y 1)]
         )))))

(defn part1 [in]
  (count (filter odd? (vals (frequencies (map parse-coordinate in))))))

(part1 inputdata); 300

(def directions
  [[(- 0 2) 0]
   [(- 0 1) (- 0 1)]
   [(- 0 1) (+ 0 1)]
   [(+ 0 2) 0]
   [(+ 0 1) (- 0 1)]
   [(+ 0 1) (+ 0 1)]])

(defn neighbours [[x y]]
  (map #(map + [x y] %) directions))

(defn count-adjacent-to [tiles [x y]]
  (count (filter odd? (map #(get tiles % 0) (neighbours [x y])))))

(defn flip-tiles [in]
  (let [in-coords (keys in)
        coords (into #{} (concat in-coords (mapcat neighbours in-coords)))]
  (loop [[x y] (first coords)
         tiles (rest coords)
         new-tiles {}]
    (if (nil? x)
      new-tiles
      (let [c (get in [x y] 0)
            adjacent-black-tiles (count-adjacent-to in [x y])]
        (cond
          (and (odd? c) ; Black tile
               (or (= adjacent-black-tiles 0)
                   (> adjacent-black-tiles 2))) (recur (first tiles) (rest tiles) (assoc new-tiles [x y] 0))
          (and (even? c) ; White tile
               (= adjacent-black-tiles 2)) (recur (first tiles) (rest tiles) (assoc new-tiles [x y] 1))
          :else (recur (first tiles) (rest tiles) (assoc new-tiles [x y] c))))))))

(defn part2 [in]
  (->> in
       (map parse-coordinate)
       (frequencies)
       (iterate flip-tiles)
       (drop 100)
       (first)
       (map second)
       (filter odd?)
       (count)))

(time (part2 testdata))
