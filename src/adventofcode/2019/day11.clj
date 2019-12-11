(ns adventofcode.2019.day11
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]))

(defn exp [x n]
  (reduce * (repeat n x)))

(def inputdata
    (vec (map #(num (BigInteger. %))
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day11/input.txt"))))
            #","))))

(defn convert-input [in]
    (zipmap (range (count in)) in))

(defn value-at-position [data, pos]
    (get data (get data pos 0) 0))

(defn parse-opcode [param])

(defn get-value [data param-mode position relative-base]
    (case param-mode
        0   (value-at-position data position)
        1   (get data position 0)
        2   (get data (+ (get data position 0) relative-base) 0)
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
                        ;(println opc)
                        ;(println (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base) (get-value data p3 (+ index 3) relative-base))
                    (case opcode
                        1   (recur
                                ; Data
                                (assoc data (alternative-get-value data p3 (+ index 3) relative-base)
                                    (+ (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base)))
                                ; Index
                                (+ index 4)
                                ; RB
                                relative-base)
                        2   (recur
                                ; Data
                                (assoc data (alternative-get-value data p3 (+ index 3) relative-base)
                                    (* (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base)))
                                ; Index
                                (+ index 4)
                                ; RB
                                relative-base)

                        3   (recur
                                ; Data
                                (assoc data (alternative-get-value data p1 (+ index 1) relative-base) (async/<! input))
                                ; Index
                                (+ index 2)
                                ; RB
                                relative-base)
                        4   (do
                                ;(println "Output " (get-value data p1 (+ index 1) relative-base))
                                (reset! last-output (get-value data p1 (+ index 1) relative-base))    ; Set last-output (For final state)
                                (async/>! output (get-value data p1 (+ index 1) relative-base))       ; Write output to output channel
                                (recur
                                    ; Data
                                    data
                                    ; Index
                                    (+ index 2)
                                    ; RB
                                    relative-base))
                        5   (recur
                                ; Data
                                data
                                ; Index
                                ; If p1 != 0, then jump to p2. Otherwise go forward
                                (if (not= (get-value data p1 (+ index 1) relative-base) 0)
                                    (get-value data p2 (+ index 2) relative-base)
                                    (+ index 3))
                                ; RB
                                relative-base)
                        6   (recur
                                ; Data
                                data
                                ; Index
                                ; If p1 == 0, then go to p2, otherwise continue
                                (if (= (get-value data p1 (+ index 1) relative-base) 0)
                                    (get-value data p2 (+ index 2) relative-base)
                                    (+ index 3))
                                ; RB
                                relative-base)
                        7   (recur
                                ; If p1 < p2; Set p3 = 1, otherwise p3 = 0
                                (if (< (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base))
                                    (assoc data (alternative-get-value data p3 (+ index 3) relative-base) 1)
                                    (assoc data (alternative-get-value data p3 (+ index 3) relative-base) 0))
                                ; Index
                                (+ index 4)
                                ; RB
                                relative-base)
                        8   (recur
                                ; If p1 == p2; Set p3 = 1, otherwise p3 = 0
                                (if (= (get-value data p1 (+ index 1) relative-base) (get-value data p2 (+ index 2) relative-base))
                                    (assoc data (alternative-get-value data p3 (+ index 3) relative-base) 1)
                                    (assoc data (alternative-get-value data p3 (+ index 3) relative-base) 0))
                                ; Index
                                (+ index 4)
                                ; RB
                                relative-base)
                        9   (recur
                                ; Data
                                data
                                ; Index
                                (+ index 2)
                                ; RB
                                (+ relative-base (get-value data p1 (+ index 1) relative-base)))
                        99   (do
                                (async/close! input)
                                (async/close! output)
                                @last-output
                                )
                        )))
                ))))

; 0 = left 90 degrees, 1 = right 90 degrees
(defn direction-from-turn [direction turn]
    (case direction
        0 (if (= turn 0) 1 3)
        1 (if (= turn 0) 2 0)
        2 (if (= turn 0) 3 1)
        3 (if (= turn 0) 0 2)
    ))

(defn step-forward [direction position]
    (case direction
        0 [(first position) (- (second position) 1)]
        1 [(- (first position) 1) (second position)]
        2 [(first position) (+ (second position) 1)]
        3 [(+ (first position) 1) (second position)]
    ))


(defn been-painted? [painting position]
    (first (map #(get % :value) (filter #(= (:pos %) position) painting))))

(defn color-of-position [painting position]
    (let [color (been-painted? painting position)]
        (if (nil? color)
            0
            (num color))))

; Direction 0^ 1< 2v 3>
(defn run-robot [input, output]
    (async/go
        (loop [painting []
                direction 0
                position [0 0]]
            (let [color (async/<! input)
                    turn (async/<! input)]
                (println "paint " color " and turn " turn)
                (println "current pos " position ", current dir " direction)
                (if (nil? color)
                    painting
                    (do
                        (async/>! output (color-of-position painting position))
                        (recur
                            (doall (concat painting [{:pos position :value color :existed (some? (been-painted? painting position)) }]))
                            (direction-from-turn direction turn)
                            (step-forward (direction-from-turn direction turn) position)
                        )
                    )
                )
            )
        )
    )
)


(def testdata (let [chans (initialize-channels [1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0] [(async/chan 1000) (async/chan 1000)])]
    (async/close! (first chans))
    (async/close! (last chans))
    (run-robot (first chans) (last chans))))

(def testrun (async/<!! testdata))

(defn borders [painting]
    (let [positions (map #(get % :pos) painting)
            x (map first positions)
            y (map second positions)]
        [
            [(apply min x) (apply max x)]
            [(apply min y) (apply max y)]
        ]
    ))

(defn paint [painting]
    (let [corners (borders painting)
            x-vals (first corners)
            y-vals (second corners)]
        (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
            (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                (let [position (last (filter #(= (:pos %) [x y]) painting))
                        color (get position :value)]
                    (if (= color 1)
                        ["#"]
                        ["."]
                    )))))))))

(defn initialize-channels [input-seq, channels]
    (last (for [i input-seq]
        (let [input (first channels)]
            (async/offer! (first channels) i)
            channels))))

;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start]
    (let [io-channels [(async/chan 1000) (async/chan 1000)]]
        (let [channels (initialize-channels start io-channels)]
            (let [robot (run-robot (last channels) (first channels))]
                (hash-map
                    :value
                    (async/<!!
                        (execute-computer (convert-input data) (first channels) (last channels)))
                    :painting
                    robot
                    :channels channels
                )))))

;(def part1
;    (run-computer inputdata [0]))

;(def part2
;    (run-computer inputdata [2]))
