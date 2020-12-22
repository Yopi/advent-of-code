(ns adventofcode.2020.day22
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as s]))

(defn parse-player-cards [cards]
  (as-> (str/split cards #"\n") c
    (subvec c 1)
    (map #(Integer. %) c)))

(defn input [file]
  (as-> (apply str ["2020/day22/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n")
    (map parse-player-cards f)
    {:player1 (vec (first f))
     :player2 (vec (second f))}
  ))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))
(def testdata2 (input "test2.txt"))

(defn play-combat [in]
  (loop [p1 (get in :player1)
         p2 (get in :player2)]
    ;(println "Player 1's deck:" p1)
    ;(println "Player 2's deck:" p2)
    (if (or (empty? p1) (empty? p2))
      (concat p1 p2)
      (let [p1-first (first p1)
            p1-rest (subvec p1 1)
            p2-first (first p2)
            p2-rest (subvec p2 1)]
        ;(println "Player 1 plays:" p1-first)
        ;(println "Player 2 plays:" p2-first)
        (cond
          (> p1-first p2-first) (recur (into [] (concat p1-rest [p1-first p2-first])) p2-rest) ; P1 wins
          :else (recur p1-rest (into [] (concat p2-rest [p2-first p1-first]))) ; P2 wins
          )))))

(defn score [cards]
  (let [scoring (reverse (range 1 (inc (count cards))))]
    (reduce + (mapv * cards scoring))))

(defn part1 [in]
  (score (play-combat in)))

(defn play-recursive-combat [in]
  (loop [p1 (get in :player1)
         p2 (get in :player2)
         played-configurations {}]
    ;(println "Player 1's deck:" p1)
    ;(println "Player 2's deck:" p2)
    (if (or (empty? p1) (empty? p2))
      [p1 p2]
      (let [p1-first (first p1)
            p1-rest (subvec p1 1)
            p2-first (first p2)
            p2-rest (subvec p2 1)
            new-plays (assoc played-configurations [p1 p2] 1)]
        ;(println "Player 1 plays:" p1-first)
        ;(println "Player 2 plays:" p2-first)
        (if (contains? played-configurations [p1 p2])
          [p1 []]
          (cond
            (and (>= (count p1-rest) p1-first)
                 (>= (count p2-rest) p2-first)) (do
                                                 ;(println "Playing a sub-game to determine the winner...")
                                                  (let [[_p1-cards p2-cards] (play-recursive-combat {:player1 (subvec p1-rest 0 p1-first) :player2 (subvec p2-rest 0 p2-first)})]
                                                  (if (empty? p2-cards)
                                                    (recur (into [] (concat p1-rest [p1-first p2-first])) p2-rest new-plays) ; P1 wins
                                                    (recur p1-rest (into [] (concat p2-rest [p2-first p1-first])) new-plays) ; P2 wins
                                                    )))
            (> p1-first p2-first) (recur (into [] (concat p1-rest [p1-first p2-first])) p2-rest new-plays) ; P1 wins
            :else (recur p1-rest (into [] (concat p2-rest [p2-first p1-first])) new-plays) ; P2 wins
            ))))))

(defn part2 [in]
  (score (apply concat (play-recursive-combat in))))
