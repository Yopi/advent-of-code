; Christmas eve
(ns adventofcode.2019.day24
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn read-input-p1 [in]
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

(defn read-input-p2 [in]
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
                {[(- x 2) (- y 2) 0] (nth (nth m y) x)})))))


(def inputdata (read-input-p1 "2019/day24/input.txt"))
(def testdata  (read-input-p1 "2019/day24/test.txt"))

(def inputdata-p2 (read-input-p2 "2019/day24/input.txt"))
(def testdata-p2  (read-input-p2 "2019/day24/test.txt"))


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

;; P2

(defn bugs-adjacent-p2 [m x y level]
    (count (filter #(= % "#")
    ;(filter #(or (= % "#") (= % "."))
    (concat
        (if (and (= x 1) (= y 0))
            [(get m [2 -2 (+ level 1)] ".")
             (get m [2 -1 (+ level 1)] ".")
             (get m [2 0 (+ level 1)] ".")
             (get m [2 1 (+ level 1)] ".")
             (get m [2 2 (+ level 1)] ".")]
            [])
        (if (and (= x -1) (= y 0))
            [(get m [-2 -2 (+ level 1)] ".")
             (get m [-2 -1 (+ level 1)] ".")
             (get m [-2 0 (+ level 1)] ".")
             (get m [-2 1 (+ level 1)] ".")
             (get m [-2 2 (+ level 1)] ".")]
            [])
        (if (and (= x 0) (= y -1))
            [(get m [-2 -2 (+ level 1)] ".")
             (get m [-1 -2 (+ level 1)] ".")
             (get m [0 -2 (+ level 1)] ".")
             (get m [1 -2 (+ level 1)] ".")
             (get m [2 -2 (+ level 1)] ".")]
            [])
        (if (and (= x 0) (= y 1))
            [(get m [-2 2 (+ level 1)] ".")
             (get m [-1 2 (+ level 1)] ".")
             (get m [0 2 (+ level 1)] ".")
             (get m [1 2 (+ level 1)] ".")
             (get m [2 2 (+ level 1)] ".")]
            [])
        (if (= x 2)
            [(get m [1 0 (- level 1)] ".")]
            [])
        (if (= x -2)
            [(get m [-1 0 (- level 1)] ".")]
            [])
        (if (= y -2)
            [(get m [0 -1 (- level 1)] ".")]
            [])
        (if (= y 2)
            [(get m [0 1 (- level 1)] ".")]
            [])
        [
            (if (or (= [(- x 1) y] [0 0]) (= x -2)) "?" (get m [(- x 1) y level] "."))
            (if (or (= [(+ x 1) y] [0 0]) (= x 2)) "?" (get m [(+ x 1) y level] "."))
            (if (or (= [x (- y 1)] [0 0]) (= y -2)) "?" (get m [x (- y 1) level] "."))
            (if (or (= [x (+ y 1)] [0 0]) (= y 2)) "?" (get m [x (+ y 1) level] "."))
        ]))
        ;)
        ))

(defn calc-tick-p2 [m]
    (let [start-x (apply min (map first (keys m)))
          start-y (apply min (map second (keys m)))
          start-level (- (apply min (map #(nth % 2) (keys m))) 1)
          h (+ (apply max (map second (keys m))) 1)
          w (+ (apply max (map first (keys m))) 1)
          max-level (+ (apply max (map #(nth % 2) (keys m))) 1)]
        (into {}
            (for [level (range (- start-level 1) (+ max-level 1))
                  y (range start-x h)
                  x (range start-y w)]
                (let [adjacent (bugs-adjacent-p2 m x y level)
                      is-bug? (= (get m [x y level]) "#")]
                    (if (= [x y] [0 0])
                        {[x y level] "?"}
                        (if is-bug?
                            (if (= adjacent 1)
                                {[x y level] "#"}
                                {[x y level] "."})
                            (if (or (= adjacent 1) (= adjacent 2))
                                {[x y level] "#"}
                                {[x y level] "."})
                    ))
               ))
        )))

(defn tick-p2 [m max-ticks]
    (loop [m m
           previous-ms []
           ticks 0]
        (let [flat-m (map second (sort-by first m))]
            (if (some #(= flat-m %) previous-ms)
                {:map m :ticks ticks :finished true}
                (if (= ticks max-ticks)
                    {:map m :ticks ticks :finished false}
                    (recur (calc-tick-p2 m) (conj previous-ms flat-m) (+ ticks 1))
                )))))

(defn paint-p2 [m]
    (let [corners [[-2 2] [-2 2]]
            x-vals (first corners)
            y-vals (second corners)
            start-level (apply min (map #(nth % 2) (keys m)))
            end-level (apply max (map #(nth % 2) (keys m)))
            ]
        (for [level (range start-level end-level)]
            (do
            (println "Depth: " level)
            (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
                (println (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                    (let [tile (get m [x y level] )]
                        [tile]
                    ))))
                )))))))

(defn part2 [m]
    (count (filter #(= % "#") (vals (get (tick-p2 m 200) :map)))))


