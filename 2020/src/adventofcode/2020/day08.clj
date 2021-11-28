(ns adventofcode.2020.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (as-> (apply str ["2020/day08/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(str/split % #" ") f)
    (vec f)))

(def testdata (input "test1.txt"))
(def inputdata (input "input.txt"))

(defn value-at-position [data, pos]
  (nth data (nth data pos)))

(defn parse-argument [arg]
  (if (= (subs arg 0 1) "+")
    (Integer. (subs arg 1))
    (- (Integer. (subs arg 1)))))

(defn lazy-contains? [coll key]
  (boolean (some #(= % key) coll)))

(defn boot [data]
  (loop [instructions data
         acc 0
         index 0
         run-indices []]
    (if (>= index (count instructions))
      {:finished true :acc acc}
      (let [operation (first (nth instructions index))
            argument (second (nth instructions index))]
        (if (lazy-contains? run-indices index)
          {:finished false :acc acc}
          (case operation
            "nop" (recur
                 ; Data
                   instructions
                 ; Acc
                   acc
                 ; Index
                   (+ index 1)
                   (concat run-indices [index]))
            "acc" (recur
                 ; Data
                   instructions
                 ; Acc
                   (+ acc (parse-argument argument))
                 ; Index
                   (+ index 1)
                   (concat run-indices [index]))
            "jmp" (recur
                 ; Data
                   instructions
                 ; Acc
                   acc
                 ; Index
                   (+ index (parse-argument argument))
                   (concat run-indices [index]))))))))

(defn part1 [d]
  (boot d))

(defn swap-jmp-nop [instruction]
  (if (= (first instruction) "jmp")
    ["nop" (second instruction)]
    (if (= (first instruction) "nop")
      ["jmp" (second instruction)]
      instruction)))

(defn part2 [d]
  (let [index (last (for [i (range (count d))
                          :let [instruction (nth d i)
                                try-booting (boot (update d i swap-jmp-nop))]
                          :while (not= (get try-booting :finished) true)]
                      i))]
    (boot (update d (inc index) swap-jmp-nop))))

(comment
  (part1 inputdata)
  (update testdata 8 swap-jmp-nop)
  (boot (update testdata 10 swap-jmp-nop))
  (part2 inputdata))
