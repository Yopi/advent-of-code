(ns adventofcode.2020.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [clojure.pprint :refer (cl-format)]))

(defn input [file]
  (as-> (apply str ["2020/day14/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")))

(def testdata (input "test1.txt"))
(def testdata2 (input "test2.txt"))
(def inputdata (input "input.txt"))

(defn update-memory [memory [row value] bitmask]
  (let [one-mask (Long/parseUnsignedLong (str/replace bitmask #"X" "1") 2)
        zero-mask (Long/parseUnsignedLong (str/replace bitmask #"X" "0") 2)
        mem-value (bit-or (bit-and one-mask (Long/parseUnsignedLong value)) zero-mask)]
    (assoc memory row mem-value)))

(defn initialize [in]
  (loop [row (first in)
         rows (rest in)
         bitmask ""
         memory {}]
    (if (nil? row)
      memory
      (cond
        (= "mask" (subs row 0 4)) (recur (first rows) (rest rows) (first (re-seq #"[X01]+" row)) memory)
        :else (recur (first rows) (rest rows) bitmask (update-memory memory (rest (first (re-seq #"mem\[(\d+)\] = (\d+)" row))) bitmask))))))

(defn part1 [in]
  (reduce + (vals (initialize in))))

(part1 inputdata)

(defn left-pad-zero [s c]
  (str
   (str/join (repeat (- c (count s)) "0"))
   s))

(defn generate-possible-bitmasks-v2 [bitmask initial-mask]
  (let [positions-of-xs (filter #(= (second %) \X) (map-indexed vector bitmask))
        masked-bitmask (str/join (apply assoc (apply vector initial-mask) (interleave (map first positions-of-xs) (repeat "X"))))]
    (as-> (str/split masked-bitmask #"X" -1) bitm
      (map vector bitm)
      (interpose ["0" "1"] bitm)
      (apply combo/cartesian-product bitm)
      (map #(apply str %) bitm))))

(defn initial-masking [bitmask original-adress-value]
  (left-pad-zero (Long/toString (bit-or (Long/parseUnsignedLong (str/replace bitmask #"X" "1") 2) original-adress-value) 2) 36))

(defn update-memory-v2 [memory [row value] bitmask]
  (let [original-adress-value (Long/parseUnsignedLong row)
        initial-mask (initial-masking bitmask original-adress-value)
        masks (generate-possible-bitmasks-v2 bitmask initial-mask)
        mem-value (Long/parseUnsignedLong value)]
    (loop [mask (first masks)
           masks (rest masks)
           memory memory]
      (if (nil? mask)
        memory
        (recur (first masks) (rest masks) (assoc memory mask mem-value))))))

(defn initialize-v2 [in]
  (loop [row (first in)
         rows (rest in)
         bitmask ""
         memory {}]
    (if (nil? row)
      memory
      (cond
        (= "mask" (subs row 0 4)) (recur (first rows) (rest rows) (first (re-seq #"[X01]+" row)) memory)
        :else (recur (first rows) (rest rows) bitmask (update-memory-v2 memory (rest (first (re-seq #"mem\[(\d+)\] = (\d+)" row))) bitmask))))))

(defn part2 [in]
  (reduce +' (vals (initialize-v2 inputdata))))

(comment
  (Long/parseUnsignedLong (str/replace (first (re-seq #"[X01]+" (first testdata))) #"X" "1") 2)
  (rest (first (re-seq #"mem\[(\d+)\] = (\d+)" (second testdata))))
  (update-memory {} (rest (first (re-seq #"mem\[(\d+)\] = (\d+)" (second testdata)))) (first (re-seq #"[X01]+" (first testdata))))
  (second testdata)
  (initialize testdata)
  (apply assoc ["a" "b" "r" "d" "r"] (interleave [2 4] (repeat "mark")))
  (generate-possible-bitmasks-v2 "000000000000000000000000000000X1001X" "000000000000000000000000000000111011")
  2r010010
  2r010011
  2r110010
  2r110011
  (bit-and 2r111011 2r110011)
  (apply combo/cartesian-product (interpose ["0" "1"] (map vector (str/split "0111X10100100X1111X11110X110X1011011" #"X" -1))))
  (str/split "0111X10100100X1111X11110X110X1011011" #"X" -1)
  (update-memory-v2 {} ["26" "1"] "XX000X0100X10X01110100011000111111X1")
  (count (generate-possible-bitmasks-v2 "XX000X0100X10X01110100011000111111X1" "110001010011010111011001100011111111"))
  (initial-masking "XX000X0100X10X01110100011000111111X1" 35042))
