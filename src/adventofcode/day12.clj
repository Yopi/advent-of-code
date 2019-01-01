(ns adventofcode.day12
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def testinput
    (str/split
      (slurp (io/file (io/resource  "day12/example.txt")))
      #"\n"))

(def realinput
    (str/split
      (slurp (io/file (io/resource  "day12/input.txt")))
      #"\n"))

(def input realinput)

(def initial-state (first (rest (re-find #"initial state: (.*)" (first input)))))

(def state-transformations (into {} (map #(str/split % #" => ") (drop 2 input))))

(def state-transformations (into {} (map (fn [[k v]] [(seq (char-array k)) (first (seq (char-array v)))])  (map #(str/split % #" => ") (drop 2 input)))))

(defn at [state pos]
    (try
        (nth state pos)
        (catch Exception e \.)))

(defn surrounding [state pos]
    [(at state (- pos 2))
    (at state (- pos 1))
    (at state pos)
    (at state (+ pos 1))
    (at state (+ pos 2))])

(defn apply-transformation [state]
    (for [i (range -3 (+ (count state) 3))]
        (get state-transformations (surrounding state i) \.)))


;; TODO:
; Detect when increase has become stable.
; Calculate the difference in generations multiplied by the increasing value.
(defn generations [state generations]
    (loop [s state
            current-gen 0]
        (if (< current-gen generations)
            (do
                (print (str "[" current-gen ": " (count-plants s current-gen) "] "))
                (println (remove-dots s))
                (recur (apply-transformation s) (inc current-gen)))
            (str/join "" s))))

(defn count-plants [state generations]
    (reduce +
        (for [i (range (count state))]
            (if (= (nth state i) \#)
                (- i (* generations 3))
                0))))

(defn remove-prefix-dots [state]
    (str/join "" (drop-while #(= \. %) state)))

(defn remove-suffix-dots [state]
    (str/reverse (remove-prefix-dots (str/reverse state))))

(defn remove-dots [state]
    (-> state
        remove-prefix-dots
        remove-suffix-dots))

(defn part1 []
    (count-plants (generations initial-state 20) 20))

(defn part2 []
    (count-plants (generations initial-state 210) 210))
