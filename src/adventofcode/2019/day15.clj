(ns adventofcode.2019.day15
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
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day15/input.txt"))))
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

(defn paint [game current-pos]
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
                            (if (= [x y] current-pos)
                                ["D"]
                                (case tile
                                    0 ["#"]
                                    1 ["."]
                                    2 ["O"]
                                    9 ["?"])))))))))]
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
            step 0
            direction "UP"]
        (let [out (output-from-direction direction)]
            (async/put! output out)
            (let [status (async/<!! input)
                    new-pos (move-robot current-pos direction)
                    new-direction (turn direction)
                    reverse-direction (turn-opposite direction)]
                (if (nil? status)
                    map ; (paint map) on every iteration
                    (case status
                        ; Hit a wall
                        0   (let [new-map (into map {new-pos 0})]
                                (recur current-pos new-map (+ step 1) (if (= turn-direction 1) new-direction reverse-direction)))

                        ; Moved a step
                        1   (let [new-map (into map {new-pos 1})]
                                (recur new-pos new-map (+ step 1) (if (= turn-direction 1) reverse-direction new-direction)))

                        ; Moved a step. Found the oxygen system
                        2   (let [new-map (into map {new-pos 2})
                                    new-direction direction]
                                new-map)
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



(defn convert-map-to-graph [map]
    (apply uber/graph 
        (apply concat ; Get rid of one level of lists
            (let [positions (set (keys (filter (comp #{1 2} last) map)))]
                (for [position positions]
                    (let [surrounding-positions (set [(move-robot position "UP") (move-robot position "LEFT") (move-robot position "DOWN") (move-robot position "RIGHT")])
                            surrounding-nodes (clojure.set/intersection surrounding-positions positions)]
                        (zipmap (vec surrounding-nodes) (repeat position))
                    ))))))

(defn start-robot [data, direction]
    (run-computer data [] direction))

(defn part1 []
    (let [map (get (start-robot inputdata 0) :map)
            position-of-oxygen (first (first (filter (comp #{2} last) map)))
            graph (convert-map-to-graph map)]
        (get (ualg/shortest-path graph [0 0] position-of-oxygen) :cost)))

(defn walk-graph-iter [graph
            node
            seen
            depth]
    (let [new-seen (into seen #{node})]
        (flatten
            (for [neighbor (uber/neighbors graph node)]
                (if (contains? seen neighbor)
                    [depth]
                    (apply max (walk-graph-iter graph neighbor new-seen (+ 1 depth))))))))

(defn walk-graph [graph root]
    (walk-graph-iter graph root #{} 0))

(defn part2 []
    (let [complete-picture (merge (get (start-robot inputdata 0) :map) (get (start-robot inputdata 1) :map))
        graph (convert-map-to-graph complete-picture)]
        (walk-graph graph [-14 16])))
