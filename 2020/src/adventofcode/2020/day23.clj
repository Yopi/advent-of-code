(ns adventofcode.2020.day23
  (:require
   [clojure.string :as str]
   [ubergraph.core :as uber]))

(defn rotate [v rotations]
  (take (count v) (drop rotations (cycle v))))

(defn parse-input [in]
  (let [cups (map #(Integer. %) (re-seq #"\d" in))]
    {:graph (apply uber/digraph (vec (zipmap cups (rotate cups 1))))
     :first-cup (first cups)
     :max-value (apply max cups)}))

(defn parse-input-extras [in]
  (let [cups (map #(Integer. %) (re-seq #"\d" in))
        m (apply max cups)
        cups-extra (concat cups (range (inc m) 1000001))
        cups-labels (concat (drop 1 cups-extra) [(first cups-extra)])]
    {:graph (apply uber/digraph (vec (zipmap cups-extra cups-labels)))
     :first-cup (first cups)
     :max-value 1000000}))

(def inputdata (parse-input "952316487"))
(def testdata (parse-input "389125467"))

(def inputdata-large (parse-input-extras "952316487"))
(def testdata-large (parse-input-extras "389125467"))


(defn get-destination-cup [cups-picked-up
                           current-cup
                           max-value]
  (loop [selected-cup (- current-cup 1)]
    (cond
      (< selected-cup 1) (recur max-value)
      (nil? (some #(= selected-cup %) cups-picked-up)) selected-cup
      :else (recur (- selected-cup 1))
      )))

(defn move [in max-moves]
  (println "Max moves: " max-moves)
  (loop [graph (get in :graph)
         max-value (get in :max-value)
         current-cup (get in :first-cup)
         move 0]
    (if (= move max-moves)
      graph
      (let [a (first (uber/successors graph current-cup))
            b (first (uber/successors graph a))
            c (first (uber/successors graph b))
            d (first (uber/successors graph c))
            dest-cup  (get-destination-cup [a b c] current-cup max-value)
            dest-cup-succ (first (uber/successors graph dest-cup))]
        ;(println "-- move " (inc move) "--")
        ;(println "current cup" current-cup)
        ;(println "pick up" [a b c])
        ;(println "destination:" dest-cup)
        (let [new-g (as-> (uber/remove-edges graph [current-cup a]) g
                      (uber/add-directed-edges g [current-cup d])
                      (uber/remove-edges g [c d])
                      (uber/remove-edges g [dest-cup dest-cup-succ])
                      (uber/add-directed-edges g [c dest-cup-succ])
                      (uber/add-directed-edges g [dest-cup a]))]
          (recur new-g
                 max-value
                 (first (uber/successors new-g current-cup))
                 (inc move)))))))

(defn get-all-nodes [g start]
  (loop [suc []
         s start]
    (let [new-suc (uber/successors g s)]
      (if (>= (.indexOf suc (first new-suc)) 0)
        suc
        (recur (concat suc new-suc) (first new-suc))))))

(defn part1 [in]
  (let [g (move in 100)]
    (str/join (drop-last 1 (get-all-nodes g 1)))))

; (time (part1 inputdata))
; "Elapsed time: 7.771961 msecs"
; "25398647"

(defn part2 [in]
  (let [g (move in 10000000)
        a (first (uber/successors g 1))
        b (first (uber/successors g a))]
    (* a b)))

; (time (part2 inputdata-large))
; "Elapsed time: 346375.467898 msecs"
; 363807398885
