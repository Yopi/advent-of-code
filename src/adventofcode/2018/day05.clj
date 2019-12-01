(ns adventofcode.2018.day05
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def inputdata
    (-> "day05/input.txt"
        io/resource
        io/file
        slurp
        str/trim))

(def testdata
    (-> "day05/example.txt"
        io/resource
        io/file
        slurp
        str/trim))

(defn cmp-lst [lst]
    (compare (first lst) (last lst)))

(defn cmp-insensitive [st]
    (cmp-lst (str/split (str/lower-case st) #"")))

(defn cmp-sensitive [st]
    (cmp-lst (str/split st #"")))


(def same-characters?
  (fn
    [item]
    (= (cmp-insensitive (first item)) 0)))

(def different-casing?
  (fn
    [item]
    (not= (cmp-sensitive (first item)) 0)))


(defn find-replacement [input]
    (first (flatten
        (filter different-casing?
            (filter same-characters?
                (->> input
                    (re-seq #"(?=(\w\w))")
                    (map rest)
                    (map #(remove nil? %))
                    (remove empty?)))))))

(defn react [old]
    (if-let [replacement (find-replacement old)]
        (str/replace old replacement "")
        old))

; Part 1
(defn solve [input, datasize]
    (let [ds (count input)]
        (if (not= ds datasize)
            (recur (react input), ds)
            input)))

(def part1
    (solve inputdata nil))

(def part1-test
    (solve testdata nil))

(def char-range
    (map char (range (int \a) (inc (int \z)))))

(defn replace-character [input, character]
    (as-> input v
        (str/replace v (str character) "")
        (str/replace v (str/capitalize (str character)) "")))

(def part2
    (->> char-range
        (map #(replace-character part1 %))
        (map #(solve % nil))
        (map count)
        (reduce min)))
