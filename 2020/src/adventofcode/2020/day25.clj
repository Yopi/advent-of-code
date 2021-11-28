(ns adventofcode.2020.day25
  (:require
   [clojure.string :as str]))

(defn parse-input [in]
  (map #(Integer. %) in))

; Card & Door
(def testdata (parse-input ["5764801" "17807724"]))
(def inputdata (parse-input ["18499292" "8790390"]))

(defn perform-step [subject-number value]
  (mod (* value subject-number) 20201227))

(defn determine-loop-size [k]
  (loop [val (perform-step 7 1)
         i 1]
    (cond
      (> i 999999999) nil
      (and (= k val) (> i 2)) i
      :else (recur (perform-step 7 val) (inc i))))
  )

(defn perform-loop [loop-size subject]
  (loop [val (perform-step subject 1)
         i 1]
    (if (= i loop-size)
      val
      (recur (perform-step subject val) (inc i)))))

(determine-loop-size (first inputdata))

(defn part1 [in]
  (let [_card-loop-size (determine-loop-size (first in))
        door-loop-size (determine-loop-size (second in))]
    (perform-loop door-loop-size (first in))
  ))

(part1 inputdata)
