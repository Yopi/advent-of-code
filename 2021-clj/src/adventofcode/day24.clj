(ns adventofcode.day24
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn read-input [in]
  (as-> (apply str ["day24/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn parse-input [in]
  (as-> in i
    (str/split i #" ")))

(def inputdata (map parse-input (read-input "input")))
(def testdata (map parse-input (read-input "testdata")))

(defn read-number-from-mem [mem n]
  (let [parsed (read-string n)]
    (if (int? parsed)
      parsed
      (Integer. (mem n 0)))))

(defn parse-instruction [input memory instruction]
  (case (first instruction)
    "inp" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (first input))
           :input (rest input)}
    "add" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (+ (read-number-from-mem memory (nth instruction 1)) (read-number-from-mem memory (nth instruction 2))))
           :input input}
    "mul" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (* (read-number-from-mem memory (nth instruction 1)) (read-number-from-mem memory (nth instruction 2))))
           :input input}
    "div" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (int (/ (read-number-from-mem memory (nth instruction 1)) (read-number-from-mem memory (nth instruction 2)))))
           :input input}
    "mod" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (mod (read-number-from-mem memory (nth instruction 1)) (read-number-from-mem memory (nth instruction 2))))
           :input input}
    "eql" {:memory (assoc
                    memory
                    (nth instruction 1)
                    (if (= (read-number-from-mem memory (nth instruction 1)) (memory (nth instruction 2) 0)) 1 0))
           :input input}))

(defn parse-instructions [instructions input]
  (loop [instruction (first instructions)
         instructions (rest instructions)
         memory {}
         input input]
    (if (nil? instruction)
      {:memory memory :input input}
      (let [{:keys [memory input]} (parse-instruction input memory instruction)]
        (recur (first instructions) (rest instructions) memory input)))))

(defn parse-relevant-instructions [instructions]
  (map
   (fn [part-input]
     (map (fn [x] (read-string (last x)))
          [(nth part-input 4)
           (nth part-input 5)
           (nth part-input 15)]))
   (partition 18 instructions)))

(defn i-to-a [i]
  (case i
    0  "A"
    1  "B"
    2  "C"
    3  "D"
    4  "E"
    5  "F"
    6  "G"
    7  "H"
    8  "I"
    9  "J"
    10 "K"
    11 "L"
    12 "M"
    13 "N"))

(defn p1-constraints [instruction instructions i stack tracker]
  (let [[a b c] instruction]
    (if (nil? a)
      tracker
      (case a
        1 (p1-constraints (first instructions) (rest instructions) (inc i) (conj stack (str (i-to-a i) " + " c)) tracker)
        26 (p1-constraints (first instructions) (rest instructions) (inc i) (pop stack) (conj tracker (str (i-to-a i) " = " (peek stack) b))) ;

        ;     (p1 (first instructions) (rest instructions) (conj (pop stack) digit))
        ;     (p1 (first instructions) (rest instructions) (pop stack)))))))
        ))))

(defn read-constraints [instructions]
  (let [relevant-instructions (parse-relevant-instructions instructions)]
    (p1-constraints (first relevant-instructions) (rest relevant-instructions) 0 [] [])))

; F = E + 7
; H = G - 7
; I = D - 2
; K = J - 3
; L = C - 8
; M = B + 5
; N = A

(defn solve []
  (for [a (range 1 10)
        b (range 1 5)  ; 1 - 4
        j (range 4 10) ; 4 - 9
        d (range 3 10) ; 3 - 9
        g (range 8 10) ; 8 - 9
        e (range 1 3) ; 1 - 2
        ]
    (let [n a
          m (+ b 5)
          c 9
          l (- c 8)
          k (- j 3)
          i (- d 2)
          u (- d 2)
          h (- g 7)
          f (+ e 7)
          model-no [a b c d e f g h i j k l m n]
          model-no-joined (read-string (str/join model-no))
          parsed (parse-instructions inputdata model-no)]
      (if (= ((parsed :memory) "z") 0)
        model-no-joined
        nil))))

(defn part1 []
  (let [x (solve)]
    {:p1 (apply max x)
     :p2 (apply min x)}
    ))
