(ns adventofcode.day21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day21/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)))

(defn parse-input [in]
  (as-> in i
    (re-seq #"\d+" i)
    {:p1 {:score 0 :loc (Integer. (nth i 1))}
     :p2 {:score 0 :loc (Integer. (nth i 3))}}))

(def inputdata (parse-input (read-input "input")))
(def testdata (parse-input (read-input "testdata")))

(defn roll-deterministic-dice [turn]
  (+ (mod turn 100) 1))

(defn new-player-location [current steps]
  (loop [step (apply + current steps)]
    (if (<= step 10)
      step
      (recur (- step 10)))))

(defn turn-deterministic [players p i]
  (let [dice-rolls (map roll-deterministic-dice (range i (+ i 3)))
        new-loc (new-player-location (get (get players p) :loc) dice-rolls)]
    (println "Player" p "rolls" dice-rolls "and moves to space" new-loc)
    (assoc-in
      (update-in players [p :score] + new-loc)
      [p :loc] new-loc)))

(defn play-deterministic [in]
  (loop [i 0
          current-player (first (keys in))
          next-player (second (keys in))
         player-state in]
    (let [new-player-state (turn-deterministic player-state current-player i)]
      (if (>= ((new-player-state current-player) :score) 1000)
        {:winner (new-player-state current-player)
         :loser (new-player-state next-player)
         :rolls (+ i 3)}
        (recur (+ i 3) next-player current-player new-player-state)
        ))))

(defn p1 [in]
  (let [{:keys [winner loser rolls]} (play-deterministic in)]
    (println loser rolls)
    (* (loser :score) rolls)))


(def roll-quantum-dice (vec (map vec (combo/selections [1 2 3] 3))))

(defn turn-quantum [players p]
  (let [dice-rolls roll-quantum-dice
        new-locs (map (fn [x] (new-player-location (get (get players p) :loc) x)) dice-rolls)]
        new-locs
    (map (fn [new-loc]
      (assoc-in
        (update-in players [p :score] + new-loc)
        [p :loc] new-loc))
      new-locs
    )))

(def turn-quantum-memoized (memoize turn-quantum))

(def play-quantum-memoized)

(defn play-quantum [player-state current-player next-player]
  (if (>= ((player-state current-player) :score) 21)
    {current-player 1}
    (if (>= ((player-state next-player) :score) 21)
      {next-player 1}
      (apply merge-with +
        (map #(play-quantum-memoized % next-player current-player) (turn-quantum-memoized player-state current-player))
  ))))

(def play-quantum-memoized (memoize play-quantum))

(defn p2 [in] (play-quantum-memoized in :p1 :p2))
