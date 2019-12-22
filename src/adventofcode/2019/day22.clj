(ns adventofcode.2019.day22
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn read-input [in]
    (as-> (clojure.java.io/resource in) i
        (slurp i)
        (str/split i #"\n")
        (map #(re-matches #"([a-z ]+) ?(-?\d+)?" %) i)
        (map #(drop 1 %) i)
        ))

(def inputdata (read-input "2019/day22/input.txt"))

(def testdata  (read-input "2019/day22/test1.txt"))
(def testdata2 (read-input "2019/day22/test2.txt"))
(def testdata3 (read-input "2019/day22/test3.txt"))
(def testdata4 (read-input "2019/day22/test4.txt"))

(defn create-deck [number-of-cards]
    (range number-of-cards))

(defn deal-into-new-stack [cards]
    (reverse cards))

(defn deal-with-increment [number cards]
    (as-> cards cards
        (map-indexed (fn [idx x] [idx x]) cards)
        (map (fn [[k v]] [(mod (* k number) (count cards)) v]) cards)
        (sort-by first cards)
        (map second cards)
        ))


(defn cut-cards-top [number cards]
    (let [top (take number cards)
            deck-without-cards (drop number cards)
            cut-deck (concat deck-without-cards top)]
        cut-deck))

(defn cut-cards-bottom [number cards]
    (let [bottom (take-last number cards)
            deck-without-cards (drop-last number cards)
            cut-deck (concat bottom deck-without-cards)]
        cut-deck))

(defn cut-cards [number cards]
    (if (>= number 0)
        (cut-cards-top number cards)
        (cut-cards-bottom (* number -1) cards)))

; (apply hash-map (map (fn [x] [x x]) (create-deck 10)))

; (map (fn [[k v]] [(mod (* k 3) 10) v]) (map (fn [x] [x x]) (create-deck 10)))

(defn part1 [input number-of-cards]
    (loop [cards (create-deck number-of-cards)
            instruction (first input)
            instructions (rest input)]
        (if (nil? instruction)
            cards
            (let [  i (str/trim (first instruction))
                    num (if (nil? (second instruction)) nil (Integer. (second instruction)))]
                (case i
                    "deal into new stack"   (recur (deal-into-new-stack cards) (first instructions) (rest instructions))
                    "deal with increment"   (recur (deal-with-increment num cards) (first instructions) (rest instructions))
                    "cut"                   (recur (cut-cards num cards) (first instructions) (rest instructions))
                )
    ))))
