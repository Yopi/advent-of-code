(ns adventofcode.2020.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as m]))

(defn to-matrix [in]
  (as-> (str/split in #"\n") rows
    (map #(str/split % #"") rows)
    (for [y (range (count rows))]
      (for [x (range (count (first rows)))]
        (nth (nth rows y) x)))))

(defn transpose [xs]
  (apply map list xs))

(defn flip [xs]
  (map reverse xs))

(defn rotate-left [xs]
  (transpose (flip xs)))

(defn input [file]
  (as-> (apply str ["2020/day20/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n")
    (map (fn [x]
           (let [l (str/split x #"\n")]
             {:tile (Integer. (first (re-seq #"\d+" (first l))))
              :picture (to-matrix (str/join "\n" (rest l)))})) f)))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

testdata

(defn side-matches? [side p1 p2]
  (cond
    (= side "L") (= (first (m/columns p1)) (last (m/columns p2)))
    (= side "R") (= (last (m/columns p1)) (first (m/columns p2)))
    (= side "U") (= (first p1) (last p2))
    (= side "D") (= (last p1) (first p2))))

(defn all-rotations [piece]
  [piece
   (rotate-left piece)
   (rotate-left (rotate-left piece))
   (rotate-left (rotate-left (rotate-left piece)))
   (flip piece)
   (rotate-left (flip piece))
   (rotate-left (rotate-left (flip piece)))
   (rotate-left (rotate-left (rotate-left (flip piece))))])

(map m/pm (all-rotations 
 (to-matrix "123
456
789
")))

(defn match-piece-against-all-sides [p1 p2]
  (let [all-rots (all-rotations p2)]
    [(not (empty? (filter true? (map (partial side-matches? "L" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "R" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "U" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "D" p1) all-rots))))]))
     
     

(any? '(false false false))
(not (empty? '(true)))
(match-piece-against-all-sides (get (first testdata) :picture) (get (second testdata) :picture))

(defn get-matches [in p1]
  (map (fn [x]
         (match-piece-against-all-sides p1 x)) (map #(get % :picture) in)))

(reduce #(or %1 %2) [true false false])

(get-matches testdata (get (first testdata) :picture))
(map (fn [x]  (reduce #(or %1 %2) x))
     [[true false false false]
      [false false false false]
      [false false true false]
      [false false false false]
      [false false false false]
      [false false false false]
      [false false false false]
      [false true false false]])

(reduce #(or %1 %2) [true false false])

(defn part1 [in]
  (loop [piece (first in)
         pieces (rest in)
         corners []]
    (if (nil? piece)
      corners
    (let [matches (get-matches in (get piece :picture))
          matching-corners (map (fn [x] (reduce #(or %1 %2) x)) matches)
          num-matches (count (filter true? matching-corners))]
      num-matches
      (if (= num-matches 3)
        (recur (first pieces) (rest pieces) (conj corners (get piece :tile)))
        (recur (first pieces) (rest pieces) corners)))
    )))

(part1 inputdata)

(reduce * [1499 1709 3083 1789])

(defn solve-puzzle [in]
  (loop [piece (first in)
         [x y] [0 0]
         rest-pieces []
         coords []
         ])
  
  )


(comment
  (to-matrix "..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
")
  (transpose [0 1 2 3]
             [4 5 6 7]
             [8 9 10 11])
  (count inputdata)
  (m/pm
   (to-matrix "..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
"))

  (m/columns (to-matrix "123
456
789
"))

  (= [1 2 3] [1 2 4])

  (rotate-left
   (rotate-left
    (rotate-left
     (to-matrix "123
456
789
")))))
