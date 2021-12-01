(ns adventofcode.2019.day05
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def testdata [1 9 10 3 2 3 11 0 99 30 40 50])

(def inputdata
    (vec (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day05/input.txt"))))
            #","))))

(defn value-at-position [data, pos]
    (nth data (nth data pos)))

(defn execute-computer [data]
    (loop [data data
            index 0]
        (let [opc (nth data index)]
            (case opc
                1   (recur (assoc data (nth data (+ index 3)) (+ (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                101  (recur (assoc data (nth data (+ index 3)) (+ (nth data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                1101 (recur (assoc data (nth data (+ index 3)) (+ (nth data (+ index 1)) (nth data (+ index 2)))) (+ index 4))
                1001 (recur (assoc data (nth data (+ index 3)) (+ (value-at-position data (+ index 1)) (nth data (+ index 2)))) (+ index 4))
                2    (recur (assoc data (nth data (+ index 3)) (* (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                 102 (recur (assoc data (nth data (+ index 3)) (* (nth data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4))
                1102 (recur (assoc data (nth data (+ index 3)) (* (nth data (+ index 1)) (nth data (+ index 2)))) (+ index 4))
                1002 (recur (assoc data (nth data (+ index 3)) (* (value-at-position data (+ index 1)) (nth data (+ index 2)))) (+ index 4))
                3    (recur (assoc data (nth data (+ index 1)) input) (+ index 2))
                4    (do (println (value-at-position data (+ index 1))) (recur data (+ index 2)))
                104  (do (println (nth data (+ index 1))) (recur data (+ index 2)))
                5    (recur data (if (not= (value-at-position data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)))
                105  (recur data (if (not= (nth data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)))
                1105 (recur data (if (not= (nth data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)))
                1005 (recur data (if (not= (value-at-position data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)))
                6    (recur data (if (= (value-at-position data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)))
                106  (recur data (if (= (nth data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)))
                1106 (recur data (if (= (nth data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)))
                1006 (recur data (if (= (value-at-position data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)))
                7    (recur (if (< (value-at-position data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                107  (recur (if (< (nth data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                1107 (recur (if (< (nth data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                1007 (recur (if (< (value-at-position data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                8    (recur (if (= (value-at-position data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                108  (recur (if (= (nth data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                1108 (recur (if (= (nth data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                1008 (recur (if (= (value-at-position data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4))
                99   data)
        )))

(defn part1 []
    (do
        (def input 1)
        (execute-computer inputdata)))

(defn part2 []
    (do
        (def input 5)
        (execute-computer inputdata)))
