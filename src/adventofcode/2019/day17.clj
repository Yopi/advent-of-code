(ns adventofcode.2019.day17
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.pprint :as pp]
        [clojure.core.async :as async]
        [ubergraph.core :as uber]
        [ubergraph.alg :as ualg]))

(def inputdata
    (vec (map #(bigint %)
        (str/split
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day17/input.txt"))))
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
                                    ;(println "Reading input" in)
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

(defn paint [game ]
    (do
        ;(print "\033[2J")
        (doall
        (if (= (count game) 0)
            []
            (doall (for [row (let [corners (borders game)
                    x-vals (first corners)
                    y-vals (second corners)]
                (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
                    (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                        (let [tile (get game [x y] 9)]
                            [tile]
                                )))))))]
                (println row)))))))

(defn output-from-direction [direction]
    (case direction
        "UP"     1
        "DOWN"   2
        "LEFT"   3
        "RIGHT"  4
    ))

(defn move-robot [current-position direction]
    (case direction
        ("UP", 1)     [(first current-position) (- (last current-position) 1)]
        ("LEFT", 2)   [(- (first current-position) 1) (last current-position)]
        ("DOWN", 3)   [(first current-position) (+ (last current-position) 1)]
        ("RIGHT", 4)  [(+ (first current-position) 1) (last current-position)]
    ))

(defn turn [direction]
    (case direction
        "UP" "RIGHT"
        "RIGHT" "DOWN"
        "DOWN" "LEFT"
        "LEFT" "UP"        
    ))

(defn turn-opposite [direction]
    (case direction
        "UP" "LEFT"
        "LEFT" "DOWN"
        "DOWN" "RIGHT"
        "RIGHT" "UP"        
    ))


(defn run-robot [input, output, turn-direction]
    (loop [current-pos [0, 0]
            map {[0 0] 1}
            step 0]
        (let [status (async/<!! input)]
            ;(paint map)
            (if (nil? status)
                map ; (paint map) on every iteration
                (let 
                    [new-pos (if (= status 10) 
                                [0 (+ (second current-pos) 1)]
                                [(+ (first current-pos) 1) (second current-pos)
                        ])]
                (case status
                    ; Draw a scaffold
                    35   (let [new-map (into map {current-pos "#"})]
                            (recur new-pos new-map (+ step 1)))

                    ; Draw an open space
                    46   (let [new-map (into map {current-pos "."})]
                            (recur new-pos new-map (+ step 1)))

                    ; Newline
                    10    (recur new-pos map (+ step 1))

                    94   (let [new-map (into map {current-pos "^"})]
                            (recur new-pos new-map (+ step 1)))
                    77  (let [new-map (into map {current-pos "M"})]
                            (recur new-pos new-map (+ step 1)))
                    (do
                        (println status)
                        (recur new-pos map (+ step 1))
                    )
                    ))))))



(defn initialize-channels [input-seq, channels]
    (if (= (count input-seq) 0)
        channels
        (last (for [i input-seq]
            (let [input (first channels) output (last channels)]
                (async/offer! (first channels) i)
                channels)))))

;; Executes computer on data with the input value of start. Single threaded
(defn run-computer [data, start, direction]
    (let [io-channels [(async/chan 1000) (async/timeout 15000)]] ; Make computer output / run-robot input channel timed out. 
        (let [channels (initialize-channels start io-channels)]  ; To prevent program from deadlocking and ease debugging.
            (hash-map
                :value (execute-computer (convert-input data) (first channels) (last channels))
                :map (run-robot (last channels) (first channels) direction)
                ))))


(defn start-robot [data]
    (run-computer data [] "UP"))

(defn get-scaffold-intersection [map]
     (partition 2 (flatten (let [corners (borders map)
                    x-vals (first corners)
                    y-vals (second corners)]
        (vec (for [y (range (first y-vals) (+ (second y-vals) 1))]
            (vec (flatten (for [x (range (first x-vals) (+ (second x-vals) 1))]
                (let [previous-x (get map [(- x 1) y] 0)
                        next-x (get map [(+ x 1) y] 0)
                        previous-y (get map [x (- y 1)] 0)
                        next-y (get map [x (+ y 1)] 0)
                        current (get map [x y] 0)]
                    (if (and 
                        (= previous-x "#")
                        (= next-x "#")
                        (= previous-y "#")
                        (= next-y "#")
                        (= current "#"))
                        [x y]
                        [])
                                ))))))))))
(defn part1 []
    (reduce + (map #(reduce * %) (get-scaffold-intersection (get (start-robot inputdata) :map)))))


(defn part2 []
    (let [main-program [65 44 65 44 67 44 66 44 67 44 66 44 67 44 65 44 66 44 65 10]
            a-routine [82 44 56 44 76 44 49 50 44 82 44 56 10]
            b-routine [76 44 49 50 44 76 44 49 50 44 76 44 49 48 44 82 44 49 48 10]
            c-routine [76 44 49 48 44 76 44 49 48 44 82 44 56 10]
            camera-feed [110 10]]
        (run-computer (assoc inputdata 0 2) (concat main-program a-routine b-routine c-routine camera-feed) "UP")))
