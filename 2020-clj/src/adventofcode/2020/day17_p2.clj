(ns adventofcode.2020.day17.p2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.math.combinatorics :as combo]))

(defn input [file]
  (as-> (apply str ["2020/day17/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(re-seq #"." %) f)
    (into {} (for [y (range (count f))
          x (range (count (first f)))]
      {[x y 0 0] (nth (nth f y) x)}))
    ))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(defn neighbours-excluding-self [[x y z w]]
  (remove #{[x y z w]}
          (apply combo/cartesian-product [[(dec x) x (inc x)]
                                          [(dec y) y (inc y)]
                                          [(dec z) z (inc z)]
                                          [(dec w) w (inc w)]])))

(defn neighbours-including-self [[x y z w]]
  (apply combo/cartesian-product [[(dec x) x (inc x)]
                                  [(dec y) y (inc y)]
                                  [(dec z) z (inc z)]
                                  [(dec w) w (inc w)]]))


(defn calc-cycle [in]
  (let [coordinates-to-consider (apply concat (map neighbours-including-self (keys in)))]
    (loop [coordinates (first coordinates-to-consider)
           cubes (rest coordinates-to-consider)
           new-state {}]
      (if (nil? coordinates)
        new-state
        (let [state (get in coordinates ".")
              neighbours (map (partial get in) (neighbours-excluding-self coordinates))
              grouped-neighbours (group-by identity neighbours)
              active-neighbours (count (get grouped-neighbours "#"))]
          (recur
           (first cubes)
           (rest cubes)
           (if (= state "#")
             (if (or (= active-neighbours 2) (= active-neighbours 3))
               (assoc new-state coordinates "#")
               new-state)
             (if (= active-neighbours 3)
               (assoc new-state coordinates "#")
               new-state))))))))

(defn run-cycles [in cycles-to-run]
  (loop [state in
         cycles 0]
    (if (= cycles cycles-to-run)
      state
      (recur (calc-cycle state) (inc cycles)))))

(defn part2 [in]
  (count (filter #(= "#" %) (vals (run-cycles in 6)))))

(time (part2 inputdata))

