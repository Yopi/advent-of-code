; Christmas eve
(ns adventofcode.2019.day24
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn read-input [in]
    (let [m
        (as-> (clojure.java.io/resource in) i
            (slurp i)
            (str/split i #"\n")
            (map #(str/split % #"") i)
            )]
        (let [h (count m)
              w (count (first m))]
            (into {} (for [y (range h)
                  x (range w)]
                {[(- x 2) (- y 2)] (nth (nth m y) x)})))))

(def inputdata (read-input "2019/day24/input.txt"))
(def testdata  (read-input "2019/day24/test.txt"))

(defn borders [map]
    (let [positions (keys map)
            x (take-nth 2 (flatten positions))
              y (take-nth 2 (drop 1 (flatten positions)))]
        [
            [(apply min x) (apply max x)]
            [(apply min y) (apply max y)]
        ]))

(defn paint [painting]
    (let [corners (borders painting)
            x-vals (first corners)
            y-vals (second corners)]
        (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
            (println (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                (let [tile (get painting [x y] )]
                    [tile]
                ))))
            )))))

(defn bugs-adjacent [m x y]
    (count (filter #(= % "#")
        [
            (get m [(- x 1) y] ".")
            (get m [(+ x 1) y] ".")
            (get m [x (- y 1)] ".")
            (get m [x (+ y 1)] ".")
        ])))

(defn calc-tick [m]
    (let [start-x (apply min (map first (keys m)))
          start-y (apply min (map second (keys m)))
          h (+ (apply max (map second (keys m))) 1)
          w (+ (apply max (map first (keys m))) 1)]
        (into {}
            (for [y (range start-x h)
                  x (range start-y w)]
                (let [adjacent (bugs-adjacent m x y)
                      is-bug? (= (get m [x y]) "#")]
                    (if is-bug?
                        (if (= adjacent 1)
                            {[x y] "#"}
                            {[x y] "."})
                        (if (or (= adjacent 1) (= adjacent 2))
                            {[x y] "#"}
                            {[x y] "."})
                    )
               ))
        )))

(defn tick [m max-ticks]
    (loop [m m
           previous-ms []
           ticks 0]
        (let [flat-m (map second (sort-by first m))]
            (if (some #(= flat-m %) previous-ms)
                {:map m :ticks ticks :finished true}
                (if (> ticks max-ticks)
                    {:map m :ticks ticks :finished false}
                    (recur (calc-tick m) (conj previous-ms flat-m) (+ ticks 1))
                )))))
(defn exp [x n]
  (reduce * (repeat n x)))

(defn biodiversity [m]
    (let [start-x (apply min (map first (keys m)))
          start-y (apply min (map second (keys m)))
          h (+ (apply max (map second (keys m))) 1)
          w (+ (apply max (map first (keys m))) 1)
          count (atom -1)]
        (reduce +
            (for [y (range start-y h)
                  x (range start-x w)
                  :let [count (swap! count inc)
                        has-bug? (= (get m [x y]) "#")]]
            (if has-bug?
                (exp 2 count)
                0)))))

(defn part1 [m]
    (biodiversity (get (tick m 100) :map)))

