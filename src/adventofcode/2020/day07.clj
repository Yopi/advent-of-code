(ns adventofcode.2020.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [ubergraph.core :as uber]
   [ubergraph.alg :as ualg]))

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn parse-number-of-bags [bag]
  (if (= bag "no ")
    0
    (Integer. bag)))

(defn input [file func]
  (as-> (apply str ["2020/day07/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/replace f #"bags" "bag")
    (str/replace f #"\." "")
    (str/split f #"\n")
    (map #(str/split % #" contain ") f)
    ;(flatten f)
    ;(apply hash-map f)
    ;(zipmap (keys f) (map #(str/split % #", ") (vals f)))
    (for [d f]
      (let [bags (map #(keyword %) (str/split (str/replace (str/replace (second d) #"\d+ *" "") #" " "-") #",-"))
            number-of-bags (map parse-number-of-bags (flatten (re-seq #"\d+|no " (second d))))]
         ;(zip (repeat (keyword (str/replace (first d) #" " "-"))) bags number-of-bags)))
        (zip bags (repeat (keyword (str/replace (first d) #" " "-"))) number-of-bags)))
    (apply concat f) ; Remove one level of nesting
    (map vec f)
    (map func f)
    (apply uber/ubergraph true false f)))

(def testdata-p1 (input "test1.txt" (fn [x] x)))
(def inputdata-p1 (input "input.txt" (fn [x] x)))

(defn part1 [g]
  (count (ualg/shortest-path g {:start-node :shiny-gold-bag :min-cost 1 :traverse true})))

(part1 inputdata-p1)

(def testdata-p2 (input "test1.txt" (fn [x] [(nth x 1) (nth x 0) (nth x 2)])))
(def testdata2-p2 (input "test2.txt" (fn [x] [(nth x 1) (nth x 0) (nth x 2)])))
(def inputdata-p2 (input "input.txt" (fn [x] [(nth x 1) (nth x 0) (nth x 2)])))


(defn bags-inside-fnc [data src]
  (let [edges (uber/find-edges data {:src src})]
    (if (and (= (count edges) 1) (= (uber/dest (first edges)) :no-other-bag))
      [0]
      (for [edge edges]
        (let [bags-inside (bags-inside-fnc data (uber/dest edge))
              number-of-bags (uber/attr data edge :weight)]
          (+ number-of-bags (* number-of-bags (reduce + bags-inside))))))))

(defn part2 [g]
  (reduce + (bags-inside-fnc g :shiny-gold-bag)))

(part2 inputdata-p2)

(comment
  (uber/graph [:light-red-bag :bright-white-bag 1] [:light-red-bag :muted-yellow-bag 2])
  (uber/pprint testdata-p1)
  (zip (repeat "light red bag") ["1 bright white bag" "2 muted yellow bag"])
  (uber/add-directed-edges (uber/graph) "a" "b")
  (map uber/attr (ualg/edges-in-path (last (ualg/shortest-path testdata-p2 {:start-node :shiny-gold-bag :traverse true}))))
  (ualg/shortest-path
   testdata-p2
   {:start-node :shiny-gold-bag :traverse true :cost-fn (fn [e] (println e) 1)})
  (ualg/bf-span testdata-p2 :shiny-gold-bag)
  (uber/dest (first (uber/find-edges testdata-p2 {:src :shiny-gold-bag}))))

