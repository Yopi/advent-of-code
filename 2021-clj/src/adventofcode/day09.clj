(ns adventofcode.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

; https://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure/3249777#3249777
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn parse-input [in]
  (as-> (apply str ["day09/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)
    (map (fn [x] (str/split x #"")) f)
    (map (fn [x] (map (fn [y] (Integer. y)) x)) f)))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn get-or-val [coll x y val] (let [row (nth coll y val)]
                               (if (number? row) row (nth row x val))))

(defn get-low-points [in]
  (keep-indexed (fn [y row]
                  (keep-indexed
                    (fn [x val]
                      (let [before-x (get-or-val in (- x 1) y 9999)
                            after-x  (get-or-val in (+ x 1) y 9999)
                            before-y (get-or-val in x (- y 1) 9999)
                            after-y  (get-or-val in x (+ y 1) 9999)]
                                     ;(println "x = " x ", val = " val " | " before-x after-x before-y after-y)
                        (if (and (< val before-x)
                              (and (< val after-x)
                                (and (< val before-y)
                                  (< val after-y))))
                          [x val]))) row))
    in))

(defn part1 [in]
  (apply +
    (map inc (map second (apply concat (get-low-points in))))))

(defn get-basin [in xy]
  (loop [x        (first xy)
         y        (second xy)
         to-check []
         basin    []]
    (let [val            (get-or-val in x y 0)
          before-x       (get-or-val in (- x 1) y 0)
          after-x        (get-or-val in (+ x 1) y 0)
          before-y       (get-or-val in x (- y 1) 0)
          after-y        (get-or-val in x (+ y 1) 0)

          added-to-check (filter seq
                           (conj
                             (conj
                               (conj
                                 (conj to-check
                                   (if (and (not (.contains to-check [(- x 1) y])) (and (not (.contains basin [(- x 1) y])) (and (not= before-x 9) (< val before-x)))) [(- x 1) y] []))
                                 (if (and (not (.contains to-check [(+ x 1) y])) (and (not (.contains basin [(+ x 1) y])) (and (not= after-x 9) (< val after-x)))) [(+ x 1) y] []))
                               (if (and (not (.contains to-check [x (- y 1)])) (and (not (.contains basin [x (- y 1)])) (and (not= before-y 9) (< val before-y)))) [x (- y 1)] []))
                             (if (and (not (.contains to-check [x (+ y 1)])) (and (not (.contains basin [x (+ y 1)])) (and (not= after-y 9) (< val after-y)))) [x (+ y 1)] [])))]
      ;(println "[" x " " y "]") 
      ;(println added-to-check)
      (if (empty? added-to-check)
        (conj basin [x y])
        (let [[next-x next-y] (first added-to-check)]
          (recur next-x next-y (rest added-to-check) (conj basin [x y])))))))

(defn part2 [in]
  (let [low-points (get-low-points in)
        locations  (apply concat (map-indexed (fn [y row] (map (fn [x] [(first x) y]) row)) low-points))
        basins     (map count
                     (map (partial get-basin in) locations))]
    (take-last 3 (sort basins))))
