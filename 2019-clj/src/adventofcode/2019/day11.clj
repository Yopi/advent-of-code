(ns adventofcode.2019.day11
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]))

(def inputdata
    (vec (map #(bigint %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day11/input.txt"))))
            #","))))

(defn convert-input [in]
    (zipmap (range (count in)) in))

(defn value-at-position [data, pos]
    (get data (get data pos 0) 0))

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
                        99  (do
                                (async/close! input)
                                (async/close! output)
                                @last-output)
                            )))
                ))))


(defn borders [painting]
    (let [positions (keys painting)
            xs (map first positions)
            ys (map second positions)]
        [[(apply min xs) (apply max xs)]
         [(apply min ys) (apply max ys)]]
    ))

(defn paint [painting]
    (for [row (let [corners (borders painting)
            x-vals (first corners)
            y-vals (second corners)]
        (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
            (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                (let [color (get painting [x y])]
                    (if (= color 1)
                        [(str "#")]
                        [(str " ")]
                    ))))))))]
        (println row)))

(defn get-current-color [painting pos]
    (get painting pos 0))

(defn turn-robot [current-direction direction]
    (if (= direction 0)
        (case current-direction 
            "UP"    "LEFT"
            "LEFT"  "DOWN"
            "DOWN"  "RIGHT"
            "RIGHT" "UP")
        (case current-direction 
            "UP"    "RIGHT"
            "RIGHT" "DOWN"
            "DOWN"  "LEFT"
            "LEFT"  "UP")))

(defn move-robot [current-position direction]
    (case direction
        "UP"     [(first current-position) (- (last current-position) 1)]
        "LEFT"   [(- (first current-position) 1) (last current-position)]
        "DOWN"   [(first current-position) (+ (last current-position) 1)]
        "RIGHT"  [(+ (first current-position) 1) (last current-position)]
    ))

; Read color and send color to output
; Get (color) and (direction) from input
; Paint color
; Turn
; Move forward
(defn robot [input, output]
    (loop [current-pos [0, 0]
            current-direction "UP"
            painting {}
            step 0]
            (let [color (async/<!! input)
                    direction (async/<!! input)]
                ;(println step)
                (if (and (nil? color) (nil? direction))
                    painting
                    (let [new-painting (into painting {current-pos color}) 
                            new-direction (turn-robot current-direction direction)
                            new-position (move-robot current-pos new-direction)]
                        (let [current-color (get-current-color painting new-position)]
                            (async/put! output current-color)
                        (recur new-position new-direction new-painting (+ step 1))
                    ))
))))

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
                :output (last channels)
                :value
                (execute-computer (convert-input data) (first channels) (last channels))
                :painting
                (robot (last channels) (first channels))
                ))))

(def part1
    (let [painting (get (run-computer inputdata [0]) :painting)]
        (paint painting)
        (count painting)))

(def part2 
    (let [painting (get (run-computer inputdata [1]) :painting)]
        (paint painting)))
