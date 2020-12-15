(ns adventofcode.2020.day15
  (:require
   [clojure.string :as str]))

(defn input-2 [in]
  (as-> (str/split in #",") i
    (map #(Long/parseLong %) i)
    (zipmap i (range 1 (inc (count i))))))

(defn input [in]
  (as-> (str/split in #",") i
        (into {} (map-indexed (fn [idx x] [(Integer. x) (inc idx)]) i))))

(def testdata (input "0,3,6"))
(def testdata2 (input "1,3,2"))
(def inputdata (input "0,6,1,7,2,19,20"))

(defn speak-number [in num-turn]
  (loop [spoken-numbers in
         turn (inc (count in))
         previous-number 0]
    ;(println "Turn: " turn)
    ;(println spoken-numbers)
    (if (= (mod turn 10000000) 0) (println "Turn: " turn))
    (if (= turn num-turn)
      previous-number
      (let [previous (get spoken-numbers previous-number)]
        ;(println previous)
        (if (nil? previous)
          (recur (assoc spoken-numbers previous-number turn) (inc turn) 0)
          (recur (assoc spoken-numbers previous-number turn) (inc turn) (- turn previous)))
        ))))


(defn part1 [in]
  (speak-number in 2020))

(time (part1 inputdata))

(defn part2 [in]
  (speak-number in 30000000))

(time (part2 inputdata))

(comment
  (- 0 (last (get {} 0)))
  (get {0 '(0) 3 '(1) 6 '(2)} 6)
  (update-in (update-in {0 '(0) 3 '(1) 6 '(2)} [0] conj 1)
             [0]
             #(drop-last 1 %))
  (drop '(1 0) 1)
  (cons 0 '(0))
  (or 1 0))

