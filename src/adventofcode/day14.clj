(ns adventofcode.day14
    (:require
        [clojure.string :as str]))

(def input 760221)

(def input-2 [7 6 0 2 2 1])

(def testanswer [3  7  1 0 1 0 1 2 4 5  1  5  8  9  1  6  7  7  9  2])


;; HELPERS

(defn nth-cycle [lst idx]
    (nth (cycle lst) idx))

(defn digits [n]
    (if (< n 10)
        [n]
        (conj (digits (quot n 10))
              (rem n 10))))

; To create new recipes, the two Elves combine their current recipes.
; This creates new recipes from the digits of the sum of the current recipes' scores.
;  With the current recipes' scores of 3 and 7, their sum is 10, and so two new recipes would be created:
;       the first with score 1 and the second with score 0.
;  If the current recipes' scores were 2 and 3, the sum, 5,
;       would only create one recipe (with a score of 5) with its single digit.

(defn create-recipe [one two]
    (let [recipe (digits (+ one two))]
        (if (empty? recipe)
            [0]
            recipe)))

(defn append-recipe [recipe one two]
    (reduce conj recipe (create-recipe one two)))

; After all new recipes are added to the scoreboard, each Elf picks a new current recipe.
; To do this,
;   the Elf steps forward through the scoreboard a number of recipes equal to 1 plus the score of their current recipe.
;   So, after the first round, the first Elf moves forward 1 + 3 = 4 times, while the second Elf moves forward 1 + 7 = 8 times.
;   If they run out of recipes, they loop back around to the beginning.
;   After the first round, both Elves happen to loop around until they land on the same recipe that they had in the beginning;
;   in general, they will move to different recipes.

(defn step [nbr]
    (loop [recipes [3 7]
            elf-1 0
            elf-2 1]
        (do
            (if (= (mod (count recipes) 10000) 0) (println (count recipes)))
            (if (> (count recipes) nbr)
                recipes
                (let [new-recipes (append-recipe recipes (nth recipes (mod elf-1 (count recipes))) (nth recipes (mod elf-2 (count recipes))))]
                    (recur new-recipes
                        (mod (+ elf-1 1 (nth new-recipes (mod elf-1 (count new-recipes)))) (count new-recipes))
                        (mod (+ elf-2 1 (nth new-recipes (mod elf-2 (count new-recipes)))) (count new-recipes))))))))

; After x get y
(defn part1 [x y]
    (str/join "" (map str (take y (drop x (step (+ x y)))))))

; Extremely slow. Most likely due to take-last on every iteration.
(defn find-combination [nbrs]
    (loop [recipes [3 7]
            elf-1 0
            elf-2 1]
        (do
            (if (or (= (mod (count recipes) 10000) 0) (= (mod (count recipes) 10000) 1)) (println (count recipes)))
            (if (or (= (take-last (count nbrs) recipes) nbrs) (= (take-last (count nbrs) (drop-last 1 recipes)) nbrs))
                (- (count recipes) (count nbrs))
                (let [new-recipes (append-recipe recipes (nth recipes (mod elf-1 (count recipes))) (nth recipes (mod elf-2 (count recipes))))]
                    (recur new-recipes
                        (mod (+ elf-1 1 (nth new-recipes (mod elf-1 (count new-recipes)))) (count new-recipes))
                        (mod (+ elf-2 1 (nth new-recipes (mod elf-2 (count new-recipes)))) (count new-recipes))))))))
