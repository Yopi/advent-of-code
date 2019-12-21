(ns adventofcode.2019.day21
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
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day21/input.txt"))))
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
                                (let [in (async/<! input)]
                                    (println "Reading input:" in)
                                    (assoc data (alternative-get-value data p1 (+ index 1) relative-base) in))
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


(defn run-robot [input, output]
    (loop [map {}
            output []
            current-pos [0 0]]
        (let [tile (async/<!! input)]
            (if (nil? tile)
                output
                (do
                    (print (str (char tile)))
                (case tile
                    10  (recur map output [0 (+ (second current-pos) 1)])
                    35  (recur (into map {current-pos "#"}) output [(+ (first current-pos) 1) (second current-pos)])
                    45  (recur (into map {current-pos "."}) output [(+ (first current-pos) 1) (second current-pos)])
                    (recur map (into output [tile]) current-pos)
                ))
            )
        )
    )
)


(defn borders [map]
    (let [positions (keys map)
            x (take-nth 2 (flatten positions))
              y (take-nth 2 (drop 1 (flatten positions)))]
        [
            [(apply min x) (apply max x)]
            [(apply min y) (apply max y)]
        ]))

(defn paint [painting]
    (let [corners (borders painting)
            x-vals (first corners)
            y-vals (second corners)]
        (println corners)
        (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
            (println (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                (let [tile (get painting [x y] )]
                    [tile]
                ))))
            )))))

(defn initialize-channels [input-seq, channels]
    (if (empty? input-seq)
        channels
        (last (for [i input-seq]
            (let [input (first channels)]
                (async/offer! (first channels) i)
                channels)))))

;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start]
    (let [io-channels [(async/chan 1000) (async/timeout 5000)]]
        (let [channels (initialize-channels start io-channels)]
            (hash-map
                :value
                (execute-computer (convert-input data) (first channels) (last channels))
                :map
                (run-robot (last channels) (first channels))
            ))))

(defn string-to-ascii [in]
    (map int (map char in)))

(defn part1 []
    (run-computer inputdata (concat
                                (string-to-ascii "NOT A J") [10]
                                (string-to-ascii "NOT B T") [10]
                                (string-to-ascii "OR T J") [10]
                                (string-to-ascii "NOT C T") [10]
                                (string-to-ascii "OR T J") [10]
                                (string-to-ascii "AND D J") [10]
                                (string-to-ascii "WALK") [10])))
