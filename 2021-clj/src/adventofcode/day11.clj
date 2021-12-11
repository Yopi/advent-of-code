(ns adventofcode.day11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn parse-input [in]
  (as-> (apply str ["day11/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)
    (map #(str/split % #"") f)
    {:data    (vec (map #(Integer. %) (flatten f)))
     :width   (count f)
     :flashes 0}))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(def testdata2 (parse-input "testdata2"))

(defn print-octopi [in]
  (pp/pprint (partition 10 in)))

(defn neighbours [in width x]
  (filter #(< % (count in))
    (filter #(>= % 0)
      [(if (= (mod x width) 0) -1 (- (- x width) 1)) ; \<
       (- x width) ; ^
       (if (= (mod x width) 9) -1 (+ (- x width) 1)) ; />
       (if (= (mod x width) 0) -1 (- x 1)) ; <
       (if (= (mod x width) 9) -1 (+ x 1)) ; >
       (if (= (mod x width) 0) -1 (- (+ x width) 1)) ; /<
       (+ x width) ; v
       (if (= (mod x width) 9) -1 (+ (+ x width) 1)) ; \>
       ])))

(defn flash [in energy-increased]
  (loop [already-flashed []
         octopi          energy-increased
         i               0]
    (let [flashing-octopi   (filter (fn [x] (not (.contains already-flashed x))) (keep-indexed (fn [idx v] (if (>= v 10) idx)) octopi))
          neighs            (map (partial neighbours (get in :data) (get in :width)) flashing-octopi)
          flashed-increased (reduce (fn [x idx] (assoc x idx (inc (nth x idx)))) octopi (flatten neighs))]
      (if (empty? flashing-octopi)
        octopi
        (recur
          (concat already-flashed flashing-octopi)
          flashed-increased
          (inc i))))))


(defn make-step [in]
  (let [energy-increased  (vec (map inc (get in :data)))
        flashed-increased (flash in energy-increased)
        reset-flashed     (map (fn [x] (if (>= x 10) 0 x)) flashed-increased)]
    {:data    reset-flashed
     :width   (get in :width)
     :flashes (+ (get in :flashes) (count (filter #(>= % 10) flashed-increased)))}))

(defn part1 [in]
  (loop [step 0
         data in]
    (if (>= step 100)
      data
      (recur (inc step) (make-step data)))))

(defn part2 [in]
  (let [zeroes (repeat 100 0)]
    (loop [step 0
           data in]
      (if (or (= 500 step) (= zeroes (get data :data)))
        step
        (recur (inc step) (make-step data))))))
