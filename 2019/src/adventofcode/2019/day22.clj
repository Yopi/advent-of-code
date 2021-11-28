(ns adventofcode.2019.day22
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [clojure.math.numeric-tower :as math]))

(defn read-input [in]
    (as-> (clojure.java.io/resource in) i
        (slurp i)
        (str/split i #"\n")
        (map #(re-matches #"([a-z ]+) ?(-?\d+)?" %) i)
        (map #(drop 1 %) i)
        ))

(def inputdata (read-input "2019/day22/input.txt"))

(def testdata  (read-input "2019/day22/test1.txt"))
(def testdata2 (read-input "2019/day22/test2.txt"))
(def testdata3 (read-input "2019/day22/test3.txt"))
(def testdata4 (read-input "2019/day22/test4.txt"))

(defn create-deck [number-of-cards]
    (range number-of-cards))

(defn deal-into-new-stack [cards]
    (reverse cards))

(defn deal-with-increment [number cards]
    (as-> cards cards
        (map-indexed (fn [idx x] [idx x]) cards)
        (map (fn [[k v]] [(mod (* k number) (count cards)) v]) cards)
        (sort-by first cards)
        (map second cards)
        ))


(defn cut-cards-top [number cards]
    (let [top (take number cards)
            deck-without-cards (drop number cards)
            cut-deck (concat deck-without-cards top)]
        cut-deck))

(defn cut-cards-bottom [number cards]
    (let [bottom (take-last number cards)
            deck-without-cards (drop-last number cards)
            cut-deck (concat bottom deck-without-cards)]
        cut-deck))

(defn cut-cards [number cards]
    (if (>= number 0)
        (cut-cards-top number cards)
        (cut-cards-bottom (* number -1) cards)))

; (apply hash-map (map (fn [x] [x x]) (create-deck 10)))

; (map (fn [[k v]] [(mod (* k 3) 10) v]) (map (fn [x] [x x]) (create-deck 10)))

(defn part1 [input number-of-cards]
    (loop [cards (create-deck number-of-cards)
            instruction (first input)
            instructions (rest input)]
        (if (nil? instruction)
            cards
            (let [  i (str/trim (first instruction))
                    num (if (nil? (second instruction)) nil (Integer. (second instruction)))]
                (case i
                    "deal into new stack"   (recur (deal-into-new-stack cards) (first instructions) (rest instructions))
                    "deal with increment"   (recur (deal-with-increment num cards) (first instructions) (rest instructions))
                    "cut"                   (recur (cut-cards num cards) (first instructions) (rest instructions))
                )
    ))))


; Modular inverses
; From Rosetta Code
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul-inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (if (= (first egcd) 1)
        (mod (second egcd) b)
        (str "No inverse since gcd is: " (first egcd)))))

(defn exp [x n]
  (reduce * (repeat n x)))

; From Rosetta Code
(defn modpow
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(def deck-size 119315717514047N)

(def final-position 2020N)

(def shuffles 101741582076661N)

(defn reverse-deal-into-new-stack [cards, position]
    (- (- cards 1) position))

(defn reverse-deal-with-increment [cards, position, deal]
    (mod (* (mul-inv deal cards) position) cards))

(defn reverse-cut [cards, position, cut]
    (mod (+ (+ position cut) cards) cards))


(defn calculate-reverse [input, position, number-of-cards]
    (let [input (reverse input)]
    (loop [position position
            instruction (first input)
            instructions (rest input)]
        (if (nil? instruction)
            position
            (let [  i (str/trim (first instruction))
                    num (if (nil? (second instruction)) nil (Integer. (second instruction)))]
                (println i num)
                (case i
                    "deal into new stack"   (recur (reverse-deal-into-new-stack number-of-cards position) (first instructions) (rest instructions))
                    "deal with increment"   (recur (reverse-deal-with-increment number-of-cards position num) (first instructions) (rest instructions))
                    "cut"                   (recur (reverse-cut number-of-cards position num) (first instructions) (rest instructions))
                )
    )))))

(defn part2 [input final-position]
    (let [one (calculate-reverse input final-position deck-size)
          two (calculate-reverse input one deck-size)]
        ; A*X + B
        (let [A (* (- one two) (mod (mul-inv (- final-position one) deck-size) deck-size))
              B (mod (- one (* A final-position)) deck-size)]
            (mod (+ (* (modpow A shuffles deck-size) final-position)
               (* (- (modpow A shuffles deck-size) 1) (* (mul-inv (- A 1) deck-size) B))) deck-size))))
