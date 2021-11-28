(ns adventofcode.2020.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))


(defn parse-input [data]
  {
   :min (Integer. (nth (str/split (nth data 0) #"-") 0))
   :max (Integer. (nth (str/split (nth data 0) #"-") 1))
   :char (let [[c](nth data 1)] c)
   :password (nth data 2)
   })

(defn input [file]
  (map parse-input (map #(str/split % #":? ")
       (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  (apply str ["2020/day02/" file]))))
        #"\n"))))

(def testdata (input "test1.txt"))
(def inputdata (input "input.txt"))

(defn valid-password [input]
  (if (let [min (get input :min)
            max (get input :max)
            count (get (frequencies (get input :password)) (get input :char) 0)]
        (and (if (>= count min) true false) (if (<= count max) true false)))
       1 0))

(def part1 (reduce + (map #(valid-password %) inputdata)))

(defn valid-password-2 [input]
  (if (let [frst (- (get input :min) 1)
        scnd (- (get input :max) 1)
        password (get input :password)
        char (get input :char)
        ]
    (or
     (and (if (= (get password frst) char) true false)
          (if (not= (get password scnd) char) true false))
     (and (if (not= (get password frst) char) true false)
          (if (= (get password scnd) char) true false)))

    ) 1 0))


(def part2 (reduce + (map #(valid-password-2 %) inputdata)))

(comment
  (get (nth testdata 0) :char)
  (nth testdata 1)
  (and (if (>= 0 1) true false) (if (<= 0 3) true false))
  (valid-password (nth testdata 0))
  (valid-password-2 (nth testdata 2))
  part1
  (map #(valid-password %) testdata)
  part2)
