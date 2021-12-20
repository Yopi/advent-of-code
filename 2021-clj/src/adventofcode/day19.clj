(ns adventofcode.day19
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as matrix]
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.pprint :as pp]))

(defrecord Node [val left right])

(defn read-input [in]
  (as-> (apply str ["day19/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn parse-input [in]
  (as-> in i
    (partition-by #(str/starts-with? % "---") i)
    (partition 2 i)
    (map (fn [[scanner positions]]
           {(last (re-find #"scanner (\d+)" (first scanner))) (map (fn [x]
                                                                     (map #(Integer. %)
                                                                       (str/split x #","))) (filter
                                                                                              #(> (count %) 0)
                                                                                              positions))}) i)
    (apply merge i)))

(def inputdata (parse-input (read-input "input")))
(def testdata (parse-input (read-input "testdata")))
(def testdata-small (parse-input (read-input "testdata-small")))

; http://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
(defn possible-rotations [[x y z]]
  [[x y z] ; reference orientation
   (map (fn [x] (int x)) (matrix/mmul [[1 0 0]
                                       [0 0 -1]
                                       [0 1 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[1 0 0]
                                       [0 -1 0]
                                       [0 0 -1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[1 0 0]
                                       [0 0 1]
                                       [0 -1 0]] [x y z]))
   ;)
   (map (fn [x] (int x)) (matrix/mmul [[0 -1 0]
                                       [1 0 0]
                                       [0 0 1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 1]
                                       [1 0 0]
                                       [0 1 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 1 0]
                                       [1 0 0]
                                       [0 0 -1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 -1]
                                       [1 0 0]
                                       [0 -1 0]] [x y z]))
   ;)
   (map (fn [x] (int x)) (matrix/mmul [[-1 0 0]
                                       [0 -1 0]
                                       [0 0 1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[-1 0 0]
                                       [0 0 -1]
                                       [0 -1 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[-1 0 0]
                                       [0 1 0]
                                       [0 0 -1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[-1 0 0]
                                       [0 0 1]
                                       [0 1 0]] [x y z]))
   ;)
   (map (fn [x] (int x)) (matrix/mmul [[0 1 0]
                                       [-1 0 0]
                                       [0 0 1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 1]
                                       [-1 0 0]
                                       [0 -1 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 -1 0]
                                       [-1 0 0]
                                       [0 0 -1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 -1]
                                       [-1 0 0]
                                       [0 1 0]] [x y z]))
   ;)
   (map (fn [x] (int x)) (matrix/mmul [[0 0 -1]
                                       [0 1 0]
                                       [1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 1 0]
                                       [0 0 1]
                                       [1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 1]
                                       [0 -1 0]
                                       [1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 -1 0]
                                       [0 0 -1]
                                       [1 0 0]] [x y z]))
  ;
   (map (fn [x] (int x)) (matrix/mmul [[0 0 -1]
                                       [0 -1 0]
                                       [-1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 -1 0]
                                       [0 0 1]
                                       [-1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 1]
                                       [0 1 0]
                                       [-1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 1 0]
                                       [0 0 -1]
                                       [-1 0 0]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[-1 0 0]
                                       [0 -1 0]
                                       [0 0 -1]] [x y z]))
   (map (fn [x] (int x)) (matrix/mmul [[0 0 1]
                                       [0 1 0]
                                       [1 0 0]] [x y z]))])

(defn all-possible-rotations [in]
  (let [possible-rotations (map possible-rotations in)]
    (loop [rotations     possible-rotations
           arranged-rots []]
      (if (empty? (first rotations))
        arranged-rots
        (recur (map (partial drop 1) rotations) (concat arranged-rots [(map first rotations)]))))))


(defn normalize-coords [[sx sy sz] in]
  (map (fn [[x y z]]
         [(- x sx) (- y sy) (- z sz)]) in))

(defn compare-coordinate-systems [a b]
  (apply merge-with +
    (for [[x1 y1 z1] a]
      (apply merge-with +
        (for [[x2 y2 z2] b]
          {[(- x2 x1) (- y2 y1) (- z2 z1)] 1})))))

(defn solve [in]
  (loop [comparison     (into #{} (in "0"))
         comparing-key  (first (rest (keys in)))
         to-be-compared (shuffle (rest (rest (keys in))))
         scanners       [[0 0 0]]]
    (if (nil? comparing-key)
      {:beacons  comparison
       :scanners scanners}
      (let [comparing          (in comparing-key)
            possible-rotations (all-possible-rotations comparing)
            normalized         (filter some?
                                 (for [rotation possible-rotations]
                                   (let [comparisons                (compare-coordinate-systems comparison rotation)
                                         scanner-positions-filtered (into {} (filter (fn [[k v]] (>= v 6)) comparisons))]
                                     (if (seq scanner-positions-filtered)
                                       (let [scanner-position (key (apply max-key val scanner-positions-filtered))]
                                         {:normalized (normalize-coords scanner-position rotation)
                                          :scanner    scanner-position})
                                       nil))))]
        (println "Comparing " comparing-key "|" "To be compared" to-be-compared)
        (if (empty? normalized)
          (recur comparison (first to-be-compared) (concat (rest to-be-compared) [comparing-key]) scanners)
          (recur (set/union comparison (into #{} ((first normalized) :normalized))) (first to-be-compared) (rest to-be-compared) (concat scanners [((first normalized) :scanner)])))))))

(defn p1 [in]
  (let [{:keys [beacons scanners]} (solve in)]
    (count beacons)))

(defn p2 [in]
  (let [{:keys [beacons scanners]} (solve in)]
    (apply max
      (for [[x1 y1 z1] scanners]
        (apply max
          (for [[x2 y2 z2] scanners]
            (+
             (Math/abs (- x1 x2))
             (Math/abs (- y1 y2))
             (Math/abs (- z1 z2)))))))))
