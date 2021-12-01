(ns adventofcode.2020.day17
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
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
      {[x y 0] (nth (nth f y) x)}))
    ))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(defn organize-cube-by-z [in]
  (let [zs (group-by (fn [x] (nth x 2)) (keys in))]
    (for [z (vals zs)]
      (let [ys (group-by (fn [x] (nth x 1)) (sort-by (fn [x] (nth x 1)) z))]
        (for [y (vals ys)]
          (for [coordinate (sort-by (fn [x] (nth x 0)) y)]
            (get in coordinate ".")))))))

(defn print-cubes [in]
  (let [lowest-z (apply min (map #(nth % 2) (keys in)))
        levels (organize-cube-by-z in)]
    (doseq [idx (range (count levels))]
      (println "Z=" (- idx lowest-z))
      (doseq [y (nth levels idx)]
        (println y)))))

(defn neighbours-excluding-self [[x y z]]
  (remove #{[x y z]}
          (apply combo/cartesian-product [[(dec x) x (inc x)]
                                          [(dec y) y (inc y)]
                                          [(dec z) z (inc z)]])))

(defn neighbours-including-self [[x y z]]
  (apply combo/cartesian-product [[(dec x) x (inc x)]
                                  [(dec y) y (inc y)]
                                  [(dec z) z (inc z)]]))


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

(defn part1 [in]
  (count (filter #(= "#" %) (vals (run-cycles in 6)))))

(part1 inputdata)
