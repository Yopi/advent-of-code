(ns adventofcode.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn all-uppercase? [s]
  (= s (str/upper-case s)))

(defn parse-input [in]
  (as-> (apply str ["day12/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)
    (map (fn [x] (str/split x #"-")) f)
    (map (fn [[x y]] {x (list y)
                      y (list x)}) f)
    (apply merge-with concat f)))

(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))
(def testdata2 (parse-input "testdata2"))
(def testdata3 (parse-input "testdata3"))

(defn search [graph visited node]
  (if (= node "end")
    1
    (as-> (get graph node) neighbours
      (filter (fn [x] (or (all-uppercase? x) (not (some visited #{x})))) neighbours)
      (map (partial search graph (conj visited node)) neighbours))))

(defn part1 [in]
  (reduce + (flatten
              (search in #{} "start"))))

(defn p2-filter [visited node]
  (let [lowercase-caves                  (filter #(not (all-uppercase? %)) (keys visited))
        visited-multiple-lowercase-caves (filter #(> % 1) (map (partial get visited) lowercase-caves))]
    (if (= node "start") false
      (if (or
            (or
              (< (get visited node 0) 1)
              (< (count visited-multiple-lowercase-caves) 1))
            (all-uppercase? node))
        true
        false))))

(defn search-p2 [graph visited node]
  (let [updated-visited (merge-with + visited {node 1})]
    (if (= node "end")
      1
      (as-> (get graph node) neighbours
        (filter (partial p2-filter updated-visited) neighbours)
        (map (partial search-p2 graph updated-visited) neighbours)))))

(defn part2 [in]
  (reduce + (flatten
              (search-p2 in {} "start"))))
