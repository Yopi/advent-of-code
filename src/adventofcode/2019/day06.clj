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

(def testdata2
    (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day06/test2.txt")))
        #"\n"))

(def nodes-in-testdata
    (seq
        (set
            (flatten
                (map #(str/split % #"\)") testdata)))))
(def test-graph (uber/graph (first nodes-in-testdata)))

(def graph-testdata
    (apply uber/add-directed-edges test-graph
        (for [orbit testdata] (str/split orbit #"\)"))))

(def nodes-in-testdata2
    (seq
        (set
            (flatten
                (map #(str/split % #"\)") testdata2)))))
(def test-graph
    (uber/graph (first nodes-in-testdata2)))

(def graph-testdata2
    (apply uber/add-edges test-graph
        (for [orbit testdata2] (str/split orbit #"\)"))))

(def nodes-in-input
    (seq
        (set
            (flatten
                (map #(str/split % #"\)") inputdata)))))
(def g
    (uber/graph
        (first nodes-in-input)))
(def graph
    (apply uber/add-directed-edges g
        (for [orbit inputdata] (str/split orbit #"\)"))))

(defn part1 [graph nodes]
    (count
        (filter not-empty
            (filter some?
                (map ualg/edges-in-path (for [x nodes y nodes] (ualg/shortest-path graph x y)))))))

(def graph-bi-directional
    (apply uber/add-edges g
        (for [orbit inputdata] (str/split orbit #"\)"))))

; Remove the first and last jump
(defn part2 [g]
    (-
        (count
            (ualg/edges-in-path
                (ualg/shortest-path g "YOU" "SAN")))
        2))
