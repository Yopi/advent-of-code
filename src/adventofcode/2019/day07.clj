(ns adventofcode.2019.day07
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]))

(defn exp [x n]
  (reduce * (repeat n x)))

; Based on https://stackoverflow.com/a/26076537
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))


(def testdata1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(def testdata2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])
(def testdata3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

(def inputdata
    (vec (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day07/input.txt"))))
            #","))))

(def input 0)

(defn value-at-position [data, pos]
    (nth data (nth data pos)))

(defn get-input []
    (let [out (first input)]
        (do
            (def input (rest input))
            out)))

(defn execute-computer [data, input]
    (loop [data data
            index 0
            input input]
        (let [opc (nth data index)]
            (case opc
                1   (recur (assoc data (nth data (+ index 3)) (+ (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4) input)
                101  (recur (assoc data (nth data (+ index 3)) (+ (nth data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4) input)
                1101 (recur (assoc data (nth data (+ index 3)) (+ (nth data (+ index 1)) (nth data (+ index 2)))) (+ index 4) input)
                1001 (recur (assoc data (nth data (+ index 3)) (+ (value-at-position data (+ index 1)) (nth data (+ index 2)))) (+ index 4) input)
                2    (recur (assoc data (nth data (+ index 3)) (* (value-at-position data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4) input)
                 102 (recur (assoc data (nth data (+ index 3)) (* (nth data (+ index 1)) (value-at-position data (+ index 2)))) (+ index 4) input)
                1102 (recur (assoc data (nth data (+ index 3)) (* (nth data (+ index 1)) (nth data (+ index 2)))) (+ index 4) input)
                1002 (recur (assoc data (nth data (+ index 3)) (* (value-at-position data (+ index 1)) (nth data (+ index 2)))) (+ index 4) input)
                3    (recur (assoc data (nth data (+ index 1)) (first input)) (+ index 2) (rest input))
                4    (do (def output (value-at-position data (+ index 1))) (recur data (+ index 2) input))
                104  (do (def output (value-at-position data (+ index 1))) (recur data (+ index 2) input))
                5    (recur data (if (not= (value-at-position data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)) input)
                105  (recur data (if (not= (nth data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)) input)
                1105 (recur data (if (not= (nth data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)) input)
                1005 (recur data (if (not= (value-at-position data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)) input)
                6    (recur data (if (= (value-at-position data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)) input)
                106  (recur data (if (= (nth data (+ index 1)) 0) (value-at-position data (+ index 2)) (+ index 3)) input)
                1106 (recur data (if (= (nth data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)) input)
                1006 (recur data (if (= (value-at-position data (+ index 1)) 0) (nth data (+ index 2)) (+ index 3)) input)
                7    (recur (if (< (value-at-position data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                107  (recur (if (< (nth data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                1107 (recur (if (< (nth data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                1007 (recur (if (< (value-at-position data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                8    (recur (if (= (value-at-position data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                108  (recur (if (= (nth data (+ index 1)) (value-at-position data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                1108 (recur (if (= (nth data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                1008 (recur (if (= (value-at-position data (+ index 1)) (nth data (+ index 2))) (assoc data (nth data (+ index 3)) 1) (assoc data (nth data (+ index 3)) 0)) (+ index 4) input)
                99   data)
        )))


(defn part1 [data]
    (do
        (def input-sequence [0, 1, 2, 3, 4])
        (for [input-sequence (permutations input-sequence)]
            (hash-map
                :sequence input-sequence
                :value
                    (last
                        (do
                        (def output 0)
                        (for [amp (range 5)]
                            (do
                                (execute-computer data [(nth input-sequence amp) output])
                                output))))))))

(def testdata1-part2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(def testdata2-part2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

(def input-sequence [9,8,7,6,5])

(def part2-setup
    [
    (hash-map :input (async/chan 100) :output (async/chan 100))
    (hash-map :input (async/chan 100) :output (async/chan 100))
    (hash-map :input (async/chan 100) :output (async/chan 100))
    (hash-map :input (async/chan 100) :output (async/chan 100))
    (hash-map :input (async/chan 100) :output (async/chan 100))
    ])

(defn previous-amp [amp]
    (if (< amp 1)
        4
        (- amp 1)))

(defn part2 [data]
    (do
        (for [i input-sequence, c (range 5)]
            (async/>! (get :input (nth part2-setup c)) i))
        (async/>! (get :input (nth part2-setup 0)) 0))

        (hash-map
            :sequence input-sequence
            :value
                (last
                    (for [amp (range 5)]
                        (execute-computer data (get :input (nth part2-setup amp)) (get :input (nth part2-setup (previous-amp amp))))))))
