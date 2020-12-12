(ns adventofcode.2020.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (as-> (apply str ["2020/day12/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map (fn [x] [(first x) (Integer. (str/join (rest x)))]) f)))

(def testdata (input "test1.txt"))
(def inputdata (input "input.txt"))

; Interleave two lists
(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn manhattan-dist [u v]
  (reduce + (map (fn [[a b]] (Math/abs (- a b))) (zip u v))))

; Part 1
(defn rotate [current-face direction len]
  (loop [current-face current-face
         len len]
    (if (= len 0)
      current-face
      (recur
       (cond
         (= direction \L)  (cond
                             (= current-face \N) \W
                             (= current-face \W) \S
                             (= current-face \S) \E
                             (= current-face \E) \N)
         (= direction \R)  (cond
                             (= current-face \N) \E
                             (= current-face \E) \S
                             (= current-face \S) \W
                             (= current-face \W) \N))
       (- len 90)))))

(defn move-ship-once [pos face coords]
  (let [[x y] pos
        [dir len] coords]
    (cond
      (= dir \N) [x (- y len) face]
      (= dir \S) [x (+ y len) face]
      (= dir \E) [(- x len) y face]
      (= dir \W) [(+ x len) y face]
      (= dir \L) [x y (rotate face dir len)]
      (= dir \R) [x y (rotate face dir len)]
      (= dir \F) (cond
                   (= face \N) [x (- y len) face]
                   (= face \S) [x (+ y len) face]
                   (= face \E) [(- x len) y face]
                   (= face \W) [(+ x len) y face]))))

(defn move-ship [coords]
  (loop [pos [0 0]
         direction \E
         current-coord (first coords)
         coords (rest coords)]
    (if (nil? current-coord)
      pos
      (let [new-pos-face (move-ship-once pos direction current-coord)
            new-pos (take 2 new-pos-face)
            new-face (last new-pos-face)]
        (recur new-pos new-face (first coords) (rest coords))))))

(defn part1 [in] (manhattan-dist (move-ship in) [0 0]))

; Part 2
(defn rotate-waypoint [[wx wy] dir len]
  (let [correct-len (cond
                      (= dir \L) (- len)
                      (= dir \R) len)
        rad (Math/toRadians correct-len)
        s (Math/sin rad)
        c (Math/cos rad)]
    [(Math/round (- (* wx c) (* wy s)))
     (Math/round (+ (* wx s) (* wy c)))]))


(defn handle-movement-once [ship-pos waypoint-pos coords]
  (let [[x y] ship-pos
        [wx wy] waypoint-pos
        [dir len] coords]
    (cond
      (= dir \N) {:ship ship-pos :waypoint [wx (- wy len)]}
      (= dir \S) {:ship ship-pos :waypoint [wx (+ wy len)]}
      (= dir \E) {:ship ship-pos :waypoint [(+ wx len) wy]}
      (= dir \W) {:ship ship-pos :waypoint [(- wx len) wy]}
      (= dir \L) {:ship ship-pos :waypoint (rotate-waypoint waypoint-pos dir len)}
      (= dir \R) {:ship ship-pos :waypoint (rotate-waypoint waypoint-pos dir len)}
      (= dir \F) {:ship [(+ x (* wx len)) (+ y (* wy len))] :waypoint waypoint-pos})))

(defn handle-movement [coords]
  (loop [pos [0 0]
         waypoint-pos [10 -1]
         current-coord (first coords)
         coords (rest coords)]
    (if (nil? current-coord)
      pos
      (let [new-pos-waypoint (handle-movement-once pos waypoint-pos current-coord)
            new-pos (get new-pos-waypoint :ship)
            new-waypoint (get new-pos-waypoint :waypoint)]
        (recur new-pos new-waypoint (first coords) (rest coords))))))

(defn part2 [in] (manhattan-dist (handle-movement in) [0 0]))

(comment
  (rotate-waypoint [10 0] \L 270)
  (handle-movement-once [0 0] [10 1] (first testdata))
  (handle-movement testdata)
  (part1 inputdata)
  (part2 inputdata))
