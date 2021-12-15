(ns adventofcode.day15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [ubergraph.core :as uber]
   [ubergraph.alg :as ualg]
   [clojure.pprint :as pp]))

(defn within-bounds [w h [x y]]
  (if (and (<= x w) (<= y h) (>= x 0) (>= y 0)) true false))

(defn wrap-nine [x]
  (if (> x 9)
    (wrap-nine (- x 9))
    x))

(defn parse-input [in]
  (as-> (apply str ["day15/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)
    (map (fn [l] (str/split l #"")) f)
    (map-indexed (fn [y l]
                   (map-indexed (fn [x c]
                                  (for [xi (range 5)
                                        yi (range 5)]
                                    {[(+ x (* xi (count l))) (+ y (* yi (count f)))] (wrap-nine (+ (Integer. c) xi yi))})) l))
      f)
    (flatten f)
    (apply merge f)
    (let [xs    (map first (keys f))
          max-x (apply max xs)
          ys    (map second (keys f))
          max-y (apply max ys)]
      (map (fn [[[x y] v]]
             (map (fn [n]
                    [[x y] n (get f n)])
               (filter (partial within-bounds max-x max-y)
                 [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]]))) f))
    (apply concat f)
    (apply uber/digraph f)))


(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn solve [in end-node]
  (let [nodes (uber/nodes in)
        xs    (map first nodes)
        max-x (apply max xs)
        ys    (map second nodes)
        max-y (apply max ys)]
    (get
      (ualg/shortest-path in   {:start-node [0 0]
                                :end-node   end-node
                                :cost-attr  :weight})
      :cost)))


; (solve testdata [9 9])
; (solve inputdata [99 99])
; (solve testdata [49 49])
; (solve inputdata [499 499])
