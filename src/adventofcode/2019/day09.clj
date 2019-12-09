(ns adventofcode.2019.day09
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]))

(defn exp [x n]
  (reduce * (repeat n x)))

(def inputdata
    (vec (map #(Integer. %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day09/input.txt"))))
            #","))))

(defn convert-input [in]
    (zipmap (range (count in)) in))

(defn value-at-position [data, pos]
    (get data (get data pos ) ))

(defn parse-opcode [param])

(defn get-value [data param-mode position relative-base]
    (case param-mode
        0   (value-at-position data position)
        1   (get data position 0)
        ;2   (get data (+ (get data position 0) relative-base) 0)
    ))


(defn alternative-get-value [data param-mode position relative-base]
    (case param-mode
        0   (get data position 0)
        1   (get data position 0)
        2   (+ (get data position 0) relative-base)))


(defn execute-computer [data, input, output]
    (async/go
        (let [last-output (atom 0)]
            (loop [data data
                    index 0
                    relative-base 0]
                (let [opc (get data index)
                        opcode (mod (get data index) 100)
                        p1 (mod (int (/ (get data index) 100)) 10)
                        p2 (mod (int (/ (get data index) 1000)) 10)
                        p3 (mod (int (/ (get data index) 10000)) 10)]
                    (do
                        (println opc)
                        (println (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base) (get-value data p3 (+ index 3) relative-base))
                        (println data)
                    (case opcode
                        1   (recur
                                ; Data
                                (assoc data (get-value data 1 (+ index 3) relative-base)
                                    (+ (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2)  relative-base)))
                                ; Index
                                (+ index 4)
                                relative-base)
                        2   (recur
                                ; Data
                                (assoc data (get-value data 1 (+ index 3) relative-base)
                                    (* (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base)))
                                ; Index
                                (+ index 4)
                                relative-base)

                        3   (recur
                                ; Get input
                                (let [in (async/<! input)]
                                    ; Data
                                    (assoc data (alternative-get-value data p1 (+ index 1) relative-base) in)
                                )
                                ; Index
                                (+ index 2)
                                relative-base)
                        4   (do
                                (println  "Output " (get-value data p1 (+ index 1) relative-base))
                                (reset! last-output (get-value data p1 (+ index 1) relative-base))    ; Set last-output (For final state)
                                (async/>! output (get-value data p1 (+ index 1) relative-base))       ; Write output to output channel
                                (recur
                                    data
                                    (+ index 2)
                                    relative-base))
                        5   (recur
                                ; Data
                                data
                                ; Index
                                ; If p1 != 0, goto p2, otherwise go forward
                                (if (not= (get-value data p1 (+ index 1) relative-base) 0)
                                    (get-value data p2 (+ index 2) relative-base)
                                    (+ index 3))
                                ; Relative base
                                relative-base)
                        6   (recur
                                ; Data
                                data
                                ; Index
                                ; If p1 == 0, goto p2, otherwise go forward
                                (if (= (get-value data p1 (+ index 1) relative-base) 0)
                                    (get-value data p2 (+ index 2) relative-base)
                                    (+ index 3))
                                ; Relative base
                                relative-base)
                        7   (recur
                                ; If p1 < p2; Set p3 = 1, otherwise p3 = 0
                                (if (< (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base))
                                    (assoc data (get-value data p3 (+ index 3) 0) 1)
                                    (assoc data (get-value data p3 (+ index 3) 0) 0))
                                ; Index
                                (+ index 4)
                                relative-base)
                        8   (recur
                                ; If p1 == p2; Set p3 = 1, otherwise p3 = 0
                                (if (= (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base))
                                    (assoc data (get-value data p3 (+ index 3) 0) 1)
                                    (assoc data (get-value data p3 (+ index 3) 0) 0))
                                ; Index
                                (+ index 4)
                                relative-base)
                        9   (recur data (+ index 2) (+ relative-base (get-value data p1 (+ index 1) relative-base)))
                        99  @last-output)
                    )
                )))))


(defn initialize-channels [input-seq, channels]
    (last (for [i input-seq]
        (let [input (first channels)]
            (async/offer! (first channels) i)
            channels))))

;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start]
    (let [io-channels [(async/chan 1000) (async/chan 1000)]]
        (let [channels (initialize-channels start io-channels)]
            (hash-map
                :value
                (async/<!!
                    (execute-computer (convert-input data) (first channels) (last channels)))))))
