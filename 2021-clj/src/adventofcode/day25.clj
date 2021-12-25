(ns adventofcode.day25
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day25/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn width [in]
  (apply max (map first (keys in))))

(defn height [in]
  (apply max (map second (keys in))))

(defn parse-input [in]
  (let [m
        (apply merge
               (map-indexed (fn [y row]
                              (as-> row i
                                (str/split i #"")
                                (map-indexed (fn [x itm] {[x y] itm}) i)
                                (apply merge i)))
                            in))]
    {
     :width (width m)
     :height (height m)
     :cucumbers (into {} (filter #(some? %) (map (fn [[idx itm]] (if (= itm ".") nil [idx itm])) m)))
    }))

(def inputdata (parse-input (read-input "input")))
(def testdata (parse-input (read-input "testdata")))
(def testdata2 (parse-input (read-input "testdata2")))

(defn next-idx [in [x y] v]
  (let [w (width in)
        h (height in)]
    (case v
      ">" (if (>= x w) [0 y] [(inc x) y])
      "v" (if (>= y h) [x 0] [x (inc y)]))))      

(defn step-fnc [dir in]
  (reduce (fn [a cuke]
            ;(println a)
            (if (= dir (second cuke))
              (let [next (next-idx in (first cuke) (second cuke))]
                ;(println cuke "-> " next " ok? " (nil? (in next)))
                (if (nil? (in next))
                  (assoc a next (second cuke))
                  (assoc a (first cuke) (second cuke))))
              (assoc a (first cuke) (second cuke))))
          {} in))
  
(defn step [in]
  (merge
   in
   {:cucumbers
    (->> (in :cucumbers)
         (step-fnc ">")
         (step-fnc "v"))}))

(defn print-map [in]
  (for [y (range 0 (inc (in :height)))]
    (println
     (for [x (range 0 (inc (in :width)))]
       ((in :cucumbers) [x y] ".")))))

(defn p1 [in]
  (loop [i 1
         cucumbers in]
    (let [next-cucumbers (step cucumbers)]
      (println i)
      (if (or (= cucumbers next-cucumbers) (= i 600))
        [i cucumbers]
        (recur (inc i) next-cucumbers)))))

(defn part1 [in]
  (time (first (p1 inputdata))))
