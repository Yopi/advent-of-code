(ns adventofcode.2020.day18-p2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn calculate [in]
  (if (= (count in) 1)
    (str (first in))
    (let [[as op bs] (take 3 in)
          a (Long. as)
          b (Long. bs)
          rest-op (drop 3 in)]
      (cond
        (= op "+") (recur (cons (+ a b) rest-op))
        (= op "*") (recur (cons (* a b) rest-op))))))

(defn calculate-order-of-operations [in]
  (if (= (count in) 1)
    (str (first in))
    (let [joined-in (str/join " " in)
          addition (re-seq #"\d+ \+ \d+" joined-in)]
      (if (nil? addition)
        (calculate in)
        (calculate-order-of-operations (str/split (str/replace joined-in #"\d+ \+ \d+" #(calculate (str/split %1 #" "))) #" "))))))

(defn calculate-group [group]
  (as-> (subs group 1 (- (count group) 1)) g
    (str/split g #" ")
    (calculate-order-of-operations g)))

(defn parse-input [in]
  (let [groups (re-seq #"\([\d +*]+\)" in)]
    (if (nil? groups)
      (str/split in #" ")
      (parse-input (str/replace in #"\([\d +*]+\)" #(calculate-group %1))))))

(defn input [file]
  (as-> (apply str ["2020/day18/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map parse-input f)))

(def inputdata (input "input.txt"))
(def testdata (parse-input "1 + 2 * 3 + 4 * 5 + 6")) ; 231
(def testdata2 (parse-input "1 + (2 * 3) + (4 * (5 + 6))")) ; 51
(def testdata3 (parse-input "2 * 3 + (4 * 5)")) ; 46
(def testdata4 (parse-input "5 + (8 * 3 + 9 + 3 * 4 * 3)")) ; 1445
(def testdata5 (parse-input "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) ; 669060
(def testdata6 (parse-input "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) ; 23340

(defn part2 [in]
  (reduce + (map #(Long. %) (map calculate-order-of-operations in))))

(part2 inputdata)
