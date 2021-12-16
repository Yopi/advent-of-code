(ns adventofcode.day16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [ubergraph.core :as uber]
   [ubergraph.alg :as ualg]
   [clojure.pprint :as pp]))

(defn to-bin [hex]
  (condp = hex
    \0 [0 0 0 0]
    \1 [0 0 0 1]
    \2 [0 0 1 0]
    \3 [0 0 1 1]
    \4 [0 1 0 0]
    \5 [0 1 0 1]
    \6 [0 1 1 0]
    \7 [0 1 1 1]
    \8 [1 0 0 0]
    \9 [1 0 0 1]
    \A [1 0 1 0]
    \B [1 0 1 1]
    \C [1 1 0 0]
    \D [1 1 0 1]
    \E [1 1 1 0]
    \F [1 1 1 1]))

(defn bin-to-dec [in]
  (Long/parseLong (apply str in) 2))

(defn read-input [in]
  (as-> (apply str ["day16/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)))

(defn parse-input [in]
  (flatten
    (map to-bin in)))

(defn all-zeroes? [in]
  (every? #(= 0 %) in))

(def inputdata (parse-input (read-input "input")))
(def testdata-2021 (parse-input "D2FE28")) ; 2021
(def testdata-two-packages (parse-input "38006F45291200")) ; 10 + 20
(def testdata-three-packages (parse-input "EE00D40C823060")) ; 1 + 2 +3
(def testdata1 (parse-input "8A004A801A8002F478"))
(def testdata2 (parse-input "620080001611562C8802118E34"))
(def testdata3 (parse-input "C0015000016115A2E0802F182340"))
(def testdata4 (parse-input "A0016C880162017C3686B18A3D4780"))
(def testdata-sum (parse-input "C200B40A82"))                ; 3
(def testdata-product (parse-input "04005AC33890"))          ; 54
(def testdata-min (parse-input "880086C3E88112"))            ; 7
(def testdata-max (parse-input "CE00C43D881120"))            ; 9
(def testdata-lt (parse-input "D8005AC2A8F0"))               ; 1
(def testdata-lt2 (parse-input "F600BC2D8F"))                ; 0
(def testdata-eq (parse-input "9C005AC2F8F0"))               ; 0
(def testdata-p2 (parse-input "9C0141080250320F1802104A08")) ; 1

(declare parse-packets)
(declare parse-packet)

(defn literal-value [in]
  (let [parts                  (partition-all 5 in)
        continued-parts        (vec (take-while #(= (first %) 1) parts))
        number-parts           (take (inc (count continued-parts)) parts)
        number-parts-excl-lead (map (partial drop 1) number-parts)
        end-parts              (drop (* 5 (inc (count continued-parts))) in)]
    {:parts     number-parts-excl-lead
     :value     (bin-to-dec (flatten number-parts-excl-lead))
     :continued end-parts}))

(defn operator [in]
  (let [length-type-id (first in)
        bits           (rest in)]
    (if (= length-type-id 0)
      (let [next-bits      (take 15 bits)
            number-of-bits (bin-to-dec next-bits)
            sub-packets    (take number-of-bits (drop 15 bits))
            remainder      (drop number-of-bits (drop 15 bits))]
        {:continued   remainder
         :sub-packets (loop [i       sub-packets
                             packets []]
                        (let [parsed (parse-packet i)]
                          (if (all-zeroes? (get parsed :continued))
                            (concat packets [parsed])
                            (recur (get parsed :continued) (concat packets [parsed])))))})
      (let [next-bits         (take 11 bits)
            number-of-packets (bin-to-dec next-bits)
            sub-packets       (loop [b       (drop 11 bits)
                                     i       0
                                     packets []]
                                (if (= i number-of-packets)
                                  packets
                                  (let [parsed (parse-packet b)]
                                    (recur (get parsed :continued) (inc i) (concat packets [parsed])))))
            remainder         (get (last sub-packets) :continued)]
        {:continued   remainder
         :sub-packets sub-packets}))))

(defn parse-packet [in]
  (let [version        (take 3 in)
        version-number (bin-to-dec version)
        type           (take 3 (drop 3 in))
        type-number    (bin-to-dec type)
        packets        (drop 6 in)]
    (merge {:version-number version-number
            :type-number    type-number}
      (let [parsed      (if
                          (= 4 type-number)
                          (literal-value packets)
                          (operator packets))
            sub-packets (get parsed :sub-packets)]
        (condp = type-number
          0 (merge
              parsed
              {:value (apply + (map #(get % :value) sub-packets))})
          1 (merge
              parsed
              {:value (apply * (map #(get % :value) sub-packets))})
          2 (merge
              parsed
              {:value (apply min (map #(get % :value) sub-packets))})
          3 (merge
              parsed
              {:value (apply max (map #(get % :value) sub-packets))})
          4 (merge
              parsed
              {:value (get parsed :value)})
          5 (merge
              parsed
              {:value (if (> (get (first sub-packets) :value) (get (second sub-packets) :value)) 1 0)})
          6 (merge
              parsed
              {:value (if (< (get (first sub-packets)  :value) (get (second sub-packets) :value)) 1 0)})
          7 (merge
              parsed
              {:value (if (= (get (first sub-packets)  :value) (get (second sub-packets) :value)) 1 0)}))))))

(defn get-version-numbers [in]
  (+ (get in :version-number)
     (apply + (map get-version-numbers (get in :sub-packets)))))

(defn part1 [in]
  (let [packets (parse-packet in)]
    (get-version-numbers packets)))

(defn part2 [in]
  (get (parse-packet in) :value))
