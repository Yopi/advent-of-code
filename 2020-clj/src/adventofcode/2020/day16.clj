(ns adventofcode.2020.day16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-valid-values [in]
  (as-> (re-seq #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)" in) posvals
    (map #(take-last 4 %) posvals)
    (map (fn [r]
           (map #(Integer. %) r)) posvals)))

(defn parse-tickets [tickets]
  (filter some?
          (for [ticket (str/split tickets #"\n")]
            (if (= (last ticket) \:)
              nil
              (map #(Integer. %) (str/split ticket #","))))))

(defn input [file]
  (as-> (apply str ["2020/day16/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n") ; 1: valid data, 2: my ticket, 3: nearby tickets
    {:valid-values (parse-valid-values (nth f 0))
     :fields (map last (re-seq #"([\w ]+):" (nth f 0)))
     :all-valid-values (apply concat (map (fn [[a b c d]] (concat (range a (inc b)) (range c (inc d)))) (parse-valid-values (nth f 0))))
     :ticket (first (parse-tickets (nth f 1)))
     :nearby-tickets (parse-tickets (nth f 2))}))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))
(def testdata2 (input "test2.txt"))

; Helpers
(defn transpose [& xs]
  (apply map list xs))

(defn indexes-of [e coll] (keep-indexed #(if (= e %2) %1) coll))

; Part 1
(defn get-invalid-fields [valid-values ticket]
  (filter some? (for [num ticket]
                  (if (false? (.contains valid-values num))
                    num
                    nil))))

(defn in-validate-tickets [in]
  (flatten (map (partial get-invalid-fields (get in :all-valid-values)) (get in :nearby-tickets))))

(defn part1 [in]
  (reduce + (in-validate-tickets in)))

; Part 2
(defn does-not-contains-invalid-field [valid-values ticket]
  (= 0 (count (filter some? (for [num ticket]
                  (if (false? (.contains valid-values num))
                    num
                    nil))))))

(defn valid-tickets [in]
  (filter (partial does-not-contains-invalid-field (get in :all-valid-values)) (get in :nearby-tickets)))

(defn all-matching [valid-values idx corresponding]
  {:idx idx
   :matching
   (for [[a b c d] valid-values]
     (every? (fn [v]
               (or (<= a v b)
                   (<= c v d))) corresponding))})

(all-matching (get testdata2 :valid-values) 0 [9 1 14])

(defn index-to-row [in]
  (let [corresponding (apply transpose (valid-tickets in))
        matching-pred (map-indexed (partial all-matching (get in :valid-values)) corresponding)
        sorted (sort-by (fn [x] (count (filter #(= true %) (get x :matching)))) matching-pred)]
    (loop [p (first sorted)
           matching (rest sorted)
           index-to-row {}]
      (if (nil? p)
        index-to-row
        (recur (first matching)
               (rest matching)
               (assoc index-to-row
                      (first (remove (set (keys index-to-row)) (indexes-of true (get p :matching))))
                      (get p :idx)
                      ))))))
(defn part2 [in]
  (let [idx-to-r (index-to-row in)]
    (apply * [(nth (get in :ticket) (get idx-to-r 0))
              (nth (get in :ticket) (get idx-to-r 1))
              (nth (get in :ticket) (get idx-to-r 2))
              (nth (get in :ticket) (get idx-to-r 3))
              (nth (get in :ticket) (get idx-to-r 4))
              (nth (get in :ticket) (get idx-to-r 5))])))

(part2 inputdata)
(valid-tickets testdata2)

