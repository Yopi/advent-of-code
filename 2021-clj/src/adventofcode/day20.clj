(ns adventofcode.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day20/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn parse-input [in]
  (as-> in i
    (let [algo (first i)
          input-image (drop 2 i)]
      [algo
       (apply merge
         (map-indexed (fn [y row]
                        (apply merge
                          (map-indexed (fn [x itm]
                                         {[x y] itm}) (str/split row #""))))
           input-image))]
      )))

(def inputdata (parse-input (read-input "input")))
(def testdata (parse-input (read-input "testdata")))

(defn point [input-image x y]
  [(get input-image [(- x 1) (- y 1)] ".")
   (get input-image [x (- y 1)] ".")
   (get input-image [(+ x 1) (- y 1)] ".")
   (get input-image [(- x 1) y] ".")
   (get input-image [x y] ".")
   (get input-image [(+ x 1) y] ".")
   (get input-image [(- x 1) (+ y 1)] ".")
   (get input-image [x (+ y 1)] ".")
   (get input-image [(+ x 1) (+ y 1)] ".")]
  )

(defn point-to-binary [point]
  (Integer/parseInt
    (str/join
      (map (fn [p]
             (if (= p ".")
               0
               1)) point)) 2))

(defn color-point [algo n]
  (condp = (nth algo n)
    \# "#"
    "."))

(defn enhance-coords [algo input-image]
  (let [xs    (map first (keys input-image))
        ys    (map second (keys input-image))
        min-x (dec (apply min xs))
        max-x (inc (apply max xs))
        min-y (dec (apply min ys))
        max-y (inc (apply min ys))]
    (apply merge-with (fn [a b] (if (or (= a "#") (= b "#")) "#" "."))
      (for [y (range (dec min-y) (inc (inc max-y)))
            x (range (dec min-x) (inc (inc max-x)))]
        {[x y] (color-point algo (point-to-binary (point input-image x y)))}))))

(defn enhance [in]
  (let [[algo input-image] in]
    ;(enhance-coords algo 
      (enhance-coords algo input-image)
    ;)
    ))
