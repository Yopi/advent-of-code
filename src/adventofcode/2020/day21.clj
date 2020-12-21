(ns adventofcode.2020.day21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as s]))

(defn input [file]
  (as-> (apply str ["2020/day21/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split-lines f)
    (map #(re-seq #"\w+|\(\)" %) f)
    (map (fn [x] (partition-by #(= "contains" %) x)) f)
    (map (fn [x] (filter #(not= (first %) "contains") x)) f)
    (map (fn [[ingredients allergens]]
           (zipmap allergens (repeat (set ingredients)))) f)))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(defn part1 [in]
  (let [allergen-mapping (apply merge-with s/intersection in)
        allergenic-ingredients (apply s/union (vals allergen-mapping))
        all-ingredients (map #(apply s/union %) (map vals in))
        non-allergenic-ingredients (map #(s/difference % allergenic-ingredients) all-ingredients)]
    (reduce + (map count non-allergenic-ingredients))))

(part1 inputdata)

(defn count-fn [[_allergen ingredients]] (count ingredients))

(defn remove-ingredient [mappings ingredient]
  [(first mappings) (s/difference (second mappings) ingredient)])

(defn pick-correct-ingredients [in]
  (let [sorted-in (sort-by count-fn in)]
    (loop [[allergen ingredients] (first sorted-in)
           allergens (rest sorted-in)
           mapping {}]
      (let [sorted-list-without-ingredient (sort-by count-fn (map #(remove-ingredient % ingredients) allergens))]
        (println [allergen ingredients])
        (println "Rest: " allergens)
        (if (nil? allergen)
          mapping
          (recur (first sorted-list-without-ingredient) (rest sorted-list-without-ingredient) (assoc mapping allergen (first ingredients))))))))

(defn part2 [in]
  (->> in
       (apply merge-with s/intersection)
       (pick-correct-ingredients)
       (sort-by first)
       (map second)
       (str/join ",")))

(part2 inputdata)
