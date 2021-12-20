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

(defn point [d input-image x y]
  [(get input-image [(- x 1) (- y 1)] d)
   (get input-image [x (- y 1)] d)
   (get input-image [(+ x 1) (- y 1)] d)
   (get input-image [(- x 1) y] d)
   (get input-image [x y] d)
   (get input-image [(+ x 1) y] d)
   (get input-image [(- x 1) (+ y 1)] d)
   (get input-image [x (+ y 1)] d)
   (get input-image [(+ x 1) (+ y 1)] d)]
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

(defn print-map [input-image]
  (let [xs    (map first (keys input-image))
        ys    (map second (keys input-image))
        min-x (dec (apply min xs))
        max-x (inc (inc (apply max xs)))
        min-y (dec (apply min ys))
        max-y (inc (inc (apply max ys)))]
        (println min-x max-x)
        (println min-y max-y)
      (map println
        (for [y (range min-y max-y)]
          (str/join
          (for [x (range min-x max-x)]
            (get input-image [x y])))))))


(defn enhance-coords [[algo input-image] dfault]
  (let [xs    (map first (keys input-image))
        ys    (map second (keys input-image))
        min-x (dec (apply min xs))
        max-x (inc (inc (apply max xs)))
        min-y (dec (apply min ys))
        max-y (inc (inc (apply max ys)))]
    (apply merge-with (fn [a b] (if (or (= a "#") (= b "#")) "#" "."))
      (for [y (range min-y max-y)
            x (range min-x max-x)]
        {[x y] (color-point algo (point-to-binary (point dfault input-image x y)))}))))

(defn enhance [in dfault]
  (let [[algo input-image] in]
  [algo
   (enhance-coords [algo input-image] dfault)]))

(defn count-lit-pixels [image]
  (count
    (filter (fn [[k v]] (= v "#"))
      image)))

(defn p1 [in]
  (count-lit-pixels
    (second
      (enhance
        (enhance in ".") "#"))))

(defn p2 [in]
  (let [dfault (cycle ["." "#"])]
    (loop [data in
           i 0]
     (if (= i 50)
       (count-lit-pixels (second data))
       (recur (enhance data (nth dfault i)) (inc i))))))
