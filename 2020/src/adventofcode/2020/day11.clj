(ns adventofcode.2020.day11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (let [seats (as-> (apply str ["2020/day11/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(str/split % #"") f))]
    (let [h (count seats)
          w (count (first seats))]
      {:seats (into {} (for [y (range h)
                             x (range w)]
                         {[x y] (nth (nth seats y) x)}))
       :width w
       :height h})))

(defn print-seats [seating]
  (for [y (range (get seating :height))]
      (for [x (range (get seating :width))]
        (let [seats (get seating :seats)]
          (get seats [x y])))))

(def testdata (input "test1.txt"))
(def inputdata (input "input.txt"))

(defn get-adjacent-seats [seats x y]
  [(get seats [x (- y 1)] ".")
   (get seats [(- x 1) (- y 1)] ".")
   (get seats [(+ x 1) (- y 1)] ".")
   (get seats [(- x 1) y] ".")
   (get seats [(+ x 1) y] ".")
   (get seats [x (+ y 1)] ".")
   (get seats [(- x 1) (+ y 1)] ".")
   (get seats [(+ x 1) (+ y 1)] ".")])

(defn count-adjacent-seats [seats x y state]
  (count (filter #(= state %) (get-adjacent-seats seats x y))))

(defn tick [seating]
  {:width (get seating :width)
   :height (get seating :height)
   :seats
   (into {} (for [y (range (get seating :height))
                  x (range (get seating :width))]
              (let [seats (get seating :seats)]
                {[x y]
                 (cond
                   (= (get seats [x y]) ".") "."
                   (and
                    (= (get seats [x y]) "#")
                    (>= (count-adjacent-seats seats x y "#") 4)) "L"
                   (and
                    (= (get seats [x y]) "L")
                    (= (count-adjacent-seats seats x y "#") 0)) "#"
                   :else (get seats [x y]))})))})

(defn get-seat [seats x-dir y-dir x y]
  (let [s (get seats [(+ x x-dir) (+ y y-dir)] " ")]
    (cond
      (= s ".") (get-seat seats x-dir y-dir (+ x x-dir) (+ y y-dir))
      :else s)))

(defn new-get-adjacent-seats [seats x y]
  [(get-seat seats 0 -1 x y)
   (get-seat seats -1 -1 x y)
   (get-seat seats +1 -1 x y)
   (get-seat seats -1 0 x y)
   (get-seat seats +1 0 x y)
   (get-seat seats 0 +1 x y)
   (get-seat seats -1 +1 x y)
   (get-seat seats +1 +1 x y)])

(defn new-count-adjacent-seats [seats x y state]
  (count (filter #(= state %) (new-get-adjacent-seats seats x y))))

(defn new-tick [seating]
  {:width (get seating :width)
   :height (get seating :height)
   :seats
   (into {} (for [y (range (get seating :height))
                  x (range (get seating :width))]
              (let [seats (get seating :seats)]
                {[x y]
                 (cond
                   (= (get seats [x y]) ".") "."
                   (and
                    (= (get seats [x y]) "#")
                    (>= (new-count-adjacent-seats seats x y "#") 5)) "L"
                   (and
                    (= (get seats [x y]) "L")
                    (= (new-count-adjacent-seats seats x y "#") 0)) "#"
                   :else (get seats [x y]))})))})

(defn tick-until-stable [seating tick-fn]
  (loop [seating seating
         previous-seating nil
         i 0]
      (if (or (= i 500) (= seating previous-seating))
        seating
        (recur (tick-fn seating) seating (inc i)))))


;(new-count-adjacent-seats (get (new-tick testdata) :seats) 0 1 "#")
;(new-get-adjacent-seats (get (new-tick testdata) :seats) 0 1)
;(print-seats testdata)
;(print-seats (new-tick (new-tick testdata)))


(defn part1 [seating]
  (count (filter #(= "#" %) (vals (get (tick-until-stable seating tick) :seats)))))

(time (part1 inputdata))

(defn part2 [seating]
  (count (filter #(= "#" %) (vals (get (tick-until-stable seating new-tick) :seats)))))

(time (part2 inputdata))

(comment
  (get (get testdata :seats) [-1 0] ".")
  (count-adjacent-seats (get testdata :seats) 0 0 "#"))
