(ns adventofcode.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn convert-to-pairs [in]
  [(first in) (Integer. (second in))])

(defn parse-input [in]
  (map convert-to-pairs
       (map #(str/split % #" ")
            (str/split
             (slurp (clojure.java.io/file (clojure.java.io/resource  in)))
             #"\n"))))

(def inputdata (parse-input "day02/input"))

(def testdata (parse-input "day02/testdata"))

(defn solve [coord, i]
  (let [dir    (first i)
        length (second i)]
    (case dir
      "forward" [(+ (first coord) length) (second coord)]
      "up"      [(first coord) (- (second coord) length)]
      "down"    [(first coord) (+ (second coord) length)])))

(defn part1 [in]
  (let [[h d] (reduce (partial mapv +) (map (partial solve [0 0]) in))]
    (* h d)))

(defn solve-part2 [in]
  (loop [pos       [0 0]
         aim       0
         positions (map (partial solve [0 0]) in)]
    (if (empty? positions)
      pos
      (let [[horiz depth] pos
            [h d]         (first positions)]
        (if (= h 0)
          (recur [horiz depth] (+ aim d) (rest positions))
          (recur [(+ horiz h) (+ depth (* aim h))] aim (rest positions)))))))

(defn part2 [in]
  (let [[h d] (solve-part2 in)]
    (* h d)))
