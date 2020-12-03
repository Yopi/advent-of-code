(ns adventofcode.2020.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [data]
  {:min (Integer. (nth (str/split (nth data 0) #"-") 0))
   :max (Integer. (nth (str/split (nth data 0) #"-") 1))
   :char (let [[c] (nth data 1)] c)
   :password (nth data 2)})

(defn input [file]
  (map #(str/split % #"")
                        (str/split
                         (slurp (clojure.java.io/file (clojure.java.io/resource  (apply str ["2020/day03/" file]))))
                         #"\n")))

(defn parse-input [data]
  {:map (for [y (range (count data)) x (range (count (first data)))]
          {:x x :y y :tree (if (= (nth (nth data y) x) "#") 1 0)})
   :width (count (first data))
   :height (count data)})

(def testdata (parse-input (input "test1.txt")))
(def inputdata (parse-input (input "input.txt")))

; Interleave two lists
(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn get-range [in x-slope y-slope]
  (zip
   (range 0 (* (get in :height) x-slope) x-slope)
   (range 0 (* (get in :height) y-slope) y-slope)))

(defn get-position [in x-act y-act]
  (let [x (mod x-act (get in :width))
        i (get in :map)]
    (filter #(= (:x %) x) 
      (filter #(= (:y %) y-act) i))))

(defn calc-trees [in r]
  (reduce + (map #(get % :tree)
                 (flatten (map #(get-position in (first %) (second %)) r)))))

(defn part1 [in]
  (calc-trees in (get-range in 3 1)))

(defn part2 [in]
  (-> (calc-trees in (get-range in 1 1))
   (* (calc-trees in (get-range in 3 1)))
   (* (calc-trees in (get-range in 5 1)))
   (* (calc-trees in (get-range in 7 1)))
   (* (calc-trees in (get-range in 1 2)))))

(comment
  testdata
  (get-range testdata 3 1)
  (get-position testdata 15 0)
  (part1 inputdata)
  (part2 testdata)
  (part2 inputdata))


