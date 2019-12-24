(ns adventofcode.2019.day23
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
            (str/trim (slurp (clojure.java.io/file (clojure.java.io/resource  "2019/day23/input.txt"))))
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
    (async/thread
        (let [last-output (atom 0)
                output-channel (atom 0)
                output-count (atom 0)]
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
                                (let [val (async/poll! input)
                                      in (if (nil? val) -1 val)]
                                    (if (not= in -1) (println "Reading:" in))
                                    (assoc data (alternative-get-value data p1 (+ index 1) relative-base) in))
                                ; Index
                                (+ index 2)
                                ; RB
                                relative-base)
                        4   (do
                                (reset! last-output (get-value data p1 (+ index 1) relative-base))    ; Set last-output (For final state)
                                (println "Output-channel:" @output-channel)
                                (if (= (mod @output-count 3) 0)
                                    (reset! output-channel (get-value data p1 (+ index 1) relative-base))
                                    (async/put! (nth output @output-channel) (get-value data p1 (+ index 1) relative-base)))       ; Write output to output channel
                                (swap! output-count inc)
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
                                (println "Stop")
                                ;(async/close! input)
                                ;(async/close! output)
                                @last-output
                                )
                        )))
                ))))

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
    (for [i (range (count channels))
            :let [c (nth channels i)
                    offer (async/put! c i)]]
        c))

(defn network-idle? [channels]
    (if (= (reduce + (map #(.count (.buf %)) channels)) 0)
        true
        false))

(defn nat [input channels]
    (async/thread
        (loop [last-x nil
               last-y nil
               delivered-xs []
               delivered-ys []]
            (if (and (network-idle? channels) (some? last-x))
                (do
                    (println "Delivering x,y:" last-x last-y)
                    (if (contains? delivered-ys last-y)
                        (println "Already delivered:" delivered-ys))
                    (async/>!! (first channels) last-x) ; Send x to 0
                    (async/>!! (first channels) last-y) ; Send y to 0
                    (recur nil nil (conj delivered-xs last-x) (conj delivered-ys last-y)))
                (let [new-x (async/poll! input)]
                    (if (nil? new-x)
                        (recur last-x last-y delivered-xs delivered-ys)
                        (let [new-y (async/<!! input)] ; We know that y will come if there was an x
                            (recur new-x new-y delivered-xs delivered-ys)))))))
)

;; Executes computer on data with the input value of start.
(defn run-computer [data, io-channels]
    (let [channels (initialize-channels [] (drop-last io-channels))]
        (hash-map
            :values
            (doall (for [c channels] (execute-computer (convert-input data) c channels)))
            :channels
            channels
            :nat
            (nat (last io-channels) channels)
        )))


;(def channels (for [i (range 50)] (async/chan 1000)))
;
;(def initialized-channels (initialize-channels [] channels))
;
;(defn run-computer [data, channels]
;            (hash-map
;                :values
;                (doall (for [c channels] (execute-computer (convert-input data) c channels)))
;                :channels
;                channels
;            ))
;

;(defn part1 []
;    (run-computer inputdata initialized-channels)
;)
;


(defn part1 []
    (run-computer inputdata (doall (for [i (range 51)] (async/chan 1000))))
)
