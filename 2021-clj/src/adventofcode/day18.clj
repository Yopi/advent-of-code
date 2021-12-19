(ns adventofcode.day18
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [clojure.pprint :as pp]))

(defrecord Node [val left right])

(defn read-input [in]
  (as-> (apply str ["day18/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(defn parse-input [in]
  (-> in
      read-string
      str))

(defn reformat [in]
  (-> in
      read-string
      str))

(def inputdata (map parse-input (read-input "input")))
(def testdata (map parse-input (read-input "testdata")))

(def test-explode (parse-input "[[[[[9,8],1],2],3],4]"))                  ;  [[[[0,9],2],3],4]
(def test-explode2 (parse-input "[7,[6,[5,[4,[3,2]]]]]"))                 ;  [7,[6,[5,[7,0]]]]
(def test-explode3 (parse-input "[[6,[5,[4,[3,2]]]],1]"))                 ;  [[6,[5,[7,0]]],3] 
(def test-explode4 (parse-input "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) ;  [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
(def test-explode5 (parse-input "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))     ;  [[3,[2,[8,0]]],[9,[5,[7,0]]]] 

(defn explode [numbers start end]
  (let [before             (subs numbers 0 start)
        after              (subs numbers end)
        [a b]              (map #(Integer. %) (re-seq #"\d+" (subs numbers start end)))
        numbers-before     (map #(Integer. %) (re-seq #"\d+" before))
        last-number-before (or (last numbers-before) 0)
        numbers-after      (map #(Integer. %) (re-seq #"\d+" after))
        first-number-after (or (first numbers-after) 0)]
    (str
      (str/replace-first before #"\d+([\s\[\]]*$)" (str " " (+ last-number-before a) " $1"))
      "0"
      (str/replace-first after #"\d+" (str " " (+ first-number-after b) " ")))))

(defn do-explosion [numbers]
  (loop [i     0
         depth 0]
    (if (= i (count numbers))
      {:exploded false
       :numbers  numbers}
      (let [c (nth numbers i)]
        (cond
          (and (>= depth 4) (= c \[)) {:exploded true
                                       :numbers  (explode numbers i (+ i (inc (count (re-find #"[\d ]+\]" (subs numbers i))))))}
          (= c \[) (recur (inc i) (inc depth))
          (= c \]) (recur (inc i) (dec depth))
          :else (recur (inc i) depth))))))

(defn do-explosions [number]
  (let [{:keys [exploded numbers]} (do-explosion (reformat number))]
    ; (println "After explosion" "\t" numbers)
    (if (not exploded)
      numbers
      (do-explosions numbers))))

(defn do-split [n]
  (let [two-digits (re-find #"\d\d" n)]
    (if (nil? two-digits)
      n
      (let [two-numbers (Integer. two-digits)
            replacement [(int (/ two-numbers 2)) (int (+ (/ two-numbers 2) 0.5))]
            splitted    (str/replace-first n (re-pattern two-digits) (str replacement))]
        ; (println "After split" "\t" "\t" splitted)
        splitted))))

(defn reduction [number]
  (loop [n (str number)]
    (let [exploded (do-explosions n)
          split    (do-split exploded)]
      (if (= exploded split)
        split
        (recur split)))))

(defn add [a b]
  ; (println "After addition " "\t" [a b])
  (reduction [(read-string a) (read-string b)]))

(defn magnitude [[a b]]
  (cond
    (and (number? a) (number? b)) (+ (* 3 a) (* 2 b))
    (number? a) (+ (* 3 a) (* 2 (magnitude b)))
    (number? b) (+ (* 3 (magnitude a)) (* 2 b))
    :else (+ (* 3 (magnitude a)) (* 2 (magnitude b)))))

(defn part1 [in]
  (magnitude (read-string (reduce add in))))

(defn part2 [in]
  (apply max
    (flatten (map (fn [[a b]]
                    (let [ab (magnitude (read-string (add a b)))
                          ba (magnitude (read-string (add b a)))]
                      [ab ba])) (combo/combinations in 2)))))
