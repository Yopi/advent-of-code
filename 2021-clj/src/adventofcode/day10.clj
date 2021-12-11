(ns adventofcode.day10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn parse-input [in]
  (as-> (apply str ["day10/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/trim-newline f)
    (str/split-lines f)))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn matching-bracket [in]
  (condp = in
    \( \)
    \) \(
    \[ \]
    \] \[
    \{ \}
    \} \{
    \< \>
    \> \<))

(defn parse [in]
  (loop [ch     (first in)
         st     (rest in)
         opened []]
    ; opening bracket
    (if (nil? ch)
      nil
      (if (some #{\( \[ \{ \<} #{ch})
        (recur (first st) (rest st) (cons ch opened))
        (let [l (first opened)]
          (if (= l (matching-bracket ch))
            (recur (first st) (rest st) (rest opened))
            ch))))))

(defn score-p1 [in]
  (condp = in
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn part1 [in]
  (apply +
    (map score-p1
      (filter some? (map parse in)))))


(defn filter-corrupt [in]
  (loop [ch     (first in)
         st     (rest in)
         opened []]
    ; opening bracket
    (if (nil? ch)
      (map matching-bracket opened)
      (if (some #{\( \[ \{ \<} #{ch})
        (recur (first st) (rest st) (cons ch opened))
        (let [l (first opened)]
          (if (= l (matching-bracket ch))
            (recur (first st) (rest st) (rest opened))
            nil))))))

(defn score-p2 [in]
  (condp = in
    \) 1
    \] 2
    \} 3
    \> 4))

(defn completions [in]
  (reduce
    (fn [sum i] (+ (* sum 5) i))
    0
    (map score-p2 in)))

(defn part2 [in]
  (let [sorted-scores  (sort
                         (map completions (filter some? (map filter-corrupt in))))]
    (nth sorted-scores (/ (count sorted-scores) 2))))
