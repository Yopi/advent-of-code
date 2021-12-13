(ns adventofcode.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn all-uppercase? [s]
  (= s (str/upper-case s)))

(defn parse-input [in]
  (as-> (apply str ["day13/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split f #"\n\n")
    (map (fn [x] (str/split-lines x)) f)
    (let [[points instructions] f]
      {:points       (apply merge
                       (map (fn [x] (let [[x y] (str/split x #",")] {[(Integer. x) (Integer. y)] 1})) points))
       :instructions (map (fn [x] (let [[_ dimension coord] (first (re-seq #"(x|y)=(\d+)" x))]
                                    {:dimension dimension
                                     :coord     (Integer. coord)})) instructions)})
  ))

(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))

(defn folding [in]
  (let [{:keys [dimension coord]} (first (get in :instructions))
        points                    (get in :points)]
    {:points (into {}
               (map (fn [[[x y] v]]
                      (condp = dimension
                        "x" (if (>= x coord)
                              [[(- (+ coord coord) x) y] v]
                              [[x y] v])
                        "y" (if (>= y coord)
                              [[x (- (+ coord coord) y)] v]
                              [[x y] v])))
                 points))
     :instructions  (rest (get in :instructions))}))

(defn part1 [in]
  (count (get (folding in) :points)))

(defn folding-p2 [in]
  (loop [in in]
    (let [{:keys [dimension coord]} (first (get in :instructions))
          points                    (get in :points)]
      (if (empty? (get in :instructions))
        points
        (recur
          (folding in))))))

(defn print-instructions [in]
  (let [xs     (map first (keys in))
        ys     (map second (keys in))
        width  (apply max xs)
        height (apply max ys)]
    (for [y (range 0 (inc height))]
      (str/join (for [x (range 0 (inc width))]
                  (if (get in [x y]) "██" "  "))))))

(defn part2 [in]
  (pp/pprint (print-instructions (folding-p2 in))))
