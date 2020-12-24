(ns adventofcode.2020.day23
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as s]))

(defn parse-input [in]
  (let [cups (map #(Integer. %) (re-seq #"\d" in))]
    cups))

(def inputdata (parse-input "952316487"))
(def testdata (parse-input "389125467"))

(defn get-destination-cup [cups-picked-up
                           current-cup
                           max-value]
  (loop [selected-cup (- current-cup 1)]
    (cond
      (< selected-cup 1) (recur max-value)
      (nil? (some #(= selected-cup %) cups-picked-up)) selected-cup
      :else (recur (- selected-cup 1))
      )))

(defn alt-subvec [v s & args]
  (let [e (or (first args) (count v))]
  (cond
    (> s e) []
    (> s (count v)) []
    (> e (count v)) []
    :else (subvec v s e)
    )
  ))

(defn rotate [v rotations]
  (take (count v) (drop rotations (cycle v))))

(defn move [in max-moves]
  (loop [i in
         move 0
         selected-index 0]
    (if (= move max-moves)
      (rotate i selected-index)
      (let [rotated-in (rotate i selected-index)
            cup (first rotated-in)
            cups (rest rotated-in)
            pick-up               (take 3 cups)
            rest-cups             (drop 3 cups)
            without-pick-up       (concat [cup] rest-cups)
            destination-cup       (get-destination-cup pick-up cup (apply max testdata))
            destination-cup-index (.indexOf without-pick-up destination-cup)
            new-cup-order         (concat
                                   (alt-subvec (vec without-pick-up) 0 destination-cup-index)
                                   [destination-cup]
                                   pick-up
                                   (alt-subvec (vec without-pick-up) (inc destination-cup-index)))
            new-selected-cup-index (mod (+ (.indexOf new-cup-order cup) 1) (count new-cup-order))]
        ;(println "-- move " (inc move) "--")
        ;(println "cups" rotated-in)
        ;(println "pick up" pick-up)
        ;(println "destination:" destination-cup "(index:" destination-cup-index ")")
        ;(println "---")
        ;(println without-pick-up)
        ;(println (alt-subvec (vec without-pick-up) 0 destination-cup-index))
        (recur
         new-cup-order
         (inc move)
         new-selected-cup-index)))))

(defn part1 [in]
  (as-> (move in 100) cups
    (rotate cups (mod (+ (.indexOf cups 1) 1) (count cups)))
    (vec cups)
    (subvec cups 0 (- (count cups) 1))
    (str/join "" cups)
    ))

(part1 inputdata)
