(ns adventofcode.2019.day06
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [ubergraph.core :as uber]
        [ubergraph.alg :as ualg]))

(def inputdata
    (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day06/input.txt")))
        #"\n"))

(def testdata
    (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day06/test.txt")))
        #"\n"))

(defn nodes-in-input [input]
    (->> input
        (map #(str/split % #"\)") ,,,)
        (flatten ,,,)
        (set ,,,)
        (seq ,,,)))

(def test-graph-directed
    (apply uber/add-directed-edges (uber/graph (first (nodes-in-input testdata)))
        (for [orbit testdata] (str/split orbit #"\)"))))

(def input-graph-directed
    (apply uber/add-directed-edges (uber/graph (first (nodes-in-input inputdata)))
        (for [orbit inputdata] (str/split orbit #"\)"))))


(defn walk-graph [graph root]
    (let [orbiters (uber/predecessors graph root)]
        (if (empty? orbiters)
            '(0)
            (for [orbiter orbiters]
                (concat '(1) (walk-graph graph orbiter))))))


(defn part1 [graph]
    (reduce + (flatten
        (for [n (uber/nodes graph)] (walk-graph graph n)))))


; Part 2
(def testdata-part2
    (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day06/test2.txt")))
        #"\n"))

(def test-graph-2-bi-directional
    (apply uber/add-edges (uber/graph (first (nodes-in-input testdata-part2)))
        (for [orbit testdata-part2] (str/split orbit #"\)"))))

(def graph-bi-directional
    (apply uber/add-edges g
        (for [orbit inputdata] (str/split orbit #"\)"))))

(def part2
    ; Remove the first and last jump
    (-
        (count
            (ualg/edges-in-path
                (ualg/shortest-path graph-bi-directional "YOU" "SAN")))
        2))
