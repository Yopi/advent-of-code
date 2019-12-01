(ns adventofcode.2018.day07
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(defn parse-prereq [s]
    (rest (re-find #"Step (\w) must be finished before step (\w) can begin." s)))

(defn parsed-data [data]
    (reduce-kv (fn [m k v] (assoc m k (map second v) ))
    {} (group-by first (map parse-prereq data))))

(def testdata
    (parsed-data
        (str/split
            (-> "day07/example.txt"
                io/resource
                io/file
                slurp
                str/trim)
            #"\n")))

(def inputdata
    (parsed-data
        (str/split
            (-> "day07/input.txt"
                io/resource
                io/file
                slurp
                str/trim)
            #"\n")))


(defn no-prereqs [possible prereqs]
    (sort (remove (set (flatten (vals prereqs))) possible)))

(defn next-no-prereq [possible prereqs]
    (str (first (no-prereqs possible prereqs))))

(defn part1-solution [input progress]
    (let [nxt (next-no-prereq (keys input) input)]
        (if (> (count input) 1)
            (recur (dissoc input nxt) (concat progress nxt))
            (apply str
                (concat
                    (concat progress (keys input))
                    (first (vals input)))))))

(def part1-test
    (part1-solution testdata ""))

(def part1
    (part1-solution inputdata ""))

; Part 2
(def workers 2)
(def time-offset 60)

(defn str-to-seconds [ch]
    (-  (-> ch
            (.getBytes)
            (first))
        65))

(defn part2-solution [input progress time-taken]
    (let [nxt (next-no-prereq (keys input) input)]
        (if (> (count input) 1)
            (recur (dissoc input nxt) (concat progress nxt) (+ time-taken (str-to-seconds nxt)))
            (+
                (+ time-taken (str-to-seconds (first (keys input))))
                (str-to-seconds (first (first (vals input))))))))


(def part2-test
    (part2-solution testdata "" 0))

(def part2
    (part2-solution inputdata "" time-offset))
