(ns adventofcode.2019.day13
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]))

(def inputdata
    (vec (map #(bigint %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day13/input.txt"))))
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
                                (let [in (async/<! input)]
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
                        99  (do
                                (async/close! input)
                                (async/close! output)
                                @last-output)
                            )))
                ))))


(defn borders [game]
    (let [positions (keys game)
            xs (map first positions)
            ys (map second positions)]
        [[(apply min xs) (apply max xs)]
         [(apply min ys) (apply max ys)]]
    ))

(defn paint [game]
    (doall
    (if (= (count game) 0)
        []
        (doall (for [row (let [corners (borders game)
                x-vals (first corners)
                y-vals (second corners)]
            (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
                (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                    (let [tile (get game [x y])]
                        (case tile
                            0 [" "]
                            1 ["#"]
                            2 ["="]
                            3 ["_"]
                            4 ["."]
                            [tile]))))))))]
            (println row))))
    (println (get game [-1 0] 0))))

(defn parse-input [in]
    (case in
        "1" -1
        "2" 0
        "3" 1
        0))

(defn arcade [input, output]
    (loop [current-pos [0, 0]
            game {}
            score 0
            step 0
            previous-tile-id 0]
            (let [x (async/<!! input)
                    y (async/<!! input)
                    tile-id (async/<!! input)]
                ;(println step)
                (if (or (nil? x) (nil? y) (nil? tile-id))
                    game ; The game is over, return game state
                    (let [new-pos [x y]
                            new-game (into game {new-pos tile-id})]
                        (let [inverted-game (clojure.set/map-invert new-game)
                                ball-pos (get inverted-game 4 [20 0])
                                paddle-pos (get inverted-game 3 [20 0])]
                            (if (= tile-id 4) ; We only want to act when we are the paddle
                                (if (< (first ball-pos) (first paddle-pos))
                                    (async/put! output -1)
                                    (if (> (first ball-pos) (first paddle-pos))
                                        (async/put! output 1)
                                        (async/put! output 0))))
                            (recur new-pos new-game score (+ step 1) tile-id)))))))


(defn initialize-channels [input-seq, channels]
    (if (= (count input-seq) 0)
        channels
        (last (for [i input-seq]
            (let [input (first channels) output (last channels)]
                (async/offer! (first channels) i)
                channels)))))

;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start]
    (let [io-channels [(async/chan 1000) (async/timeout 10000)]] ; Make comptuer output / arcade input channel timed out. 
        (let [channels (initialize-channels start io-channels)]  ; To prevent program from deadlocking and ease debugging.
            (hash-map
                :value (execute-computer (convert-input data) (first channels) (last channels))
                :game (arcade (last channels) (first channels))
                ))))

(defn part1 []
    (let [game (get (run-computer inputdata [0]) :game)]
        (count (filter #(= 2 %) (vals game)))))

(defn part2 []
    (let [game (get (run-computer inputdata []) :game)]
        (str "Score: " (get game [-1 0]))))
