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
(def testdata1-part2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(def testdata2-part2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

(def inputdata
    (vec (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day07/input.txt"))))
            #","))))

(defn value-at-position [data, pos]
    (nth data (nth data pos)))

(defn parse-opcode [param])

(defn get-value [data param-mode position]
    (if (= param-mode 0)
        (value-at-position data position)
        (nth data position)))


(defn execute-computer [data, input, output]
    (async/go
        (let [last-output (atom 0)]
            (loop [data data
                    index 0]
                (let [opc (nth data index)
                        opcode (mod (nth data index) 100)
                        p1 (mod (int (/ (nth data index) 100)) 10)
                        p2 (mod (int (/ (nth data index) 1000)) 10)
                        p3 (mod (int (/ (nth data index) 10000)) 10)]
                    (case opcode
                        1   (recur
                                ; Data
                                (assoc data (get-value data 1 (+ index 3))
                                    (+ (get-value data p1 (+ index 1)) (get-value data p2 (+ index 2))))
                                ; Index
                                (+ index 4))
                        2   (recur
                                ; Data
                                (assoc data (get-value data 1 (+ index 3))
                                    (* (get-value data p1 (+ index 1)) (get-value data p2 (+ index 2))))
                                ; Index
                                (+ index 4))

                        3   (recur
                                ; Data
                                (assoc data (nth data (+ index 1)) (async/<! input))
                                ; Index
                                (+ index 2))
                        4   (do
                                (reset! last-output (get-value data p1 (+ index 1)))    ; Set last-output (For final state)
                                (async/>! output (get-value data p1 (+ index 1)))       ; Write output to output channel
                                (recur
                                    data
                                    (+ index 2)))
                        5   (recur data
                                (if (not= (get-value data p1 (+ index 1)) 0)
                                    (get-value data p2 (+ index 2))
                                    (+ index 3)))
                        6   (recur data
                                (if (= (get-value data p1 (+ index 1)) 0)
                                    (get-value data p2 (+ index 2))
                                    (+ index 3)))
                        7   (recur
                                ; If p1 < p2; Set p3 = 1, otherwise p3 = 0
                                (if (< (get-value data p1 (+ index 1)) (get-value data p2 (+ index 2)))
                                    (assoc data (nth data (+ index 3)) 1)
                                    (assoc data (nth data (+ index 3)) 0))
                                ; Index
                                (+ index 4))
                        8   (recur
                                ; If p1 == p2; Set p3 = 1, otherwise p3 = 0
                                (if (= (get-value data p1 (+ index 1)) (get-value data p2 (+ index 2)))
                                    (assoc data (nth data (+ index 3)) 1)
                                    (assoc data (nth data (+ index 3)) 0))
                                ; Index
                                (+ index 4))
                        99   @last-output))
                ))))

(defn next-amp [amp, max-amps]
    (if (> amp 3)
        0
        (+ amp 1)))

(defn initialize-channels [input-seq, channels]
    (for [c (range (count input-seq))]
        (let [i (nth input-seq c)]
            (async/offer! (nth channels c) i)
            (if (= 0 c)
                (async/offer! (nth channels 0) 0)
                false)
            (nth channels c))))


;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start]
    (let [input-channels [(async/chan 1000) (async/chan 1000)]]
        (let [channels (initialize-channels [start] input-channels)]
            (hash-map
                :value
                (async/<!!
                    (execute-computer data (first channels) (last channels)))))))

(defn solution [data input-sequence]
        (for [input-sequence (permutations input-sequence)]
            (let [input-channels [(async/chan 1000), (async/chan 1000), (async/chan 1000), (async/chan 1000), (async/chan 1000)]]
                (let [channels (initialize-channels input-sequence input-channels)]
                (hash-map
                    :sequence input-sequence
                    :value
                        (async/<!!
                        (last
                            (for [amp (range (count input-sequence))]
                                (execute-computer data (nth channels amp) (nth channels (next-amp amp (count input-sequence))))))))))))

(def part1
    (apply max-key :value
        (solution inputdata [0, 1, 2, 3, 4])))

(def part2
    (apply max-key :value
        (solution inputdata [9,8,7,6,5])))
