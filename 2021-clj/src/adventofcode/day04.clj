(ns adventofcode.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

; https://stackoverflow.com/questions/40153970/transpose-a-list-of-lists-clojure/40154124#40154124
(defn transpose [& xs]
  (apply map list xs))

(defn parse-numbers [numbers]
  (str/split numbers #","))
;)

(defn parse-board [fnc board]
  (as-> board b
    (map #(str/split % #" ") b)
    (map (fn [x] (filter seq x)) b)
    (apply fnc b)
    (map #(into #{} %) b)
    ))

(defn parse-boards [fnc boards]
  (as-> (partition-by #(= (.length %) 0) boards) b
    (filter #(> (.size %) 1) b)
    (map (partial parse-board fnc) b)))

(defn parse-input [in]
  (as-> (apply str ["day04/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split-lines f)
    (let [numbers (first f)
          boards  (rest (rest f))]
      {:numbers     (parse-numbers numbers)
       :boards-rows (parse-boards (fn [& xs] xs) boards)
       :boards-cols (parse-boards transpose boards)})))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn winning-board? [numbers board]
  (let [number-set (into #{} numbers)]
    (if (> (.size
            (filter true? (map #(set/subset? % number-set) board)))
           0)
      board
      nil)))

(defn get-winning-board [in]
  (loop [i 1]
    (let [numbers (take i (get in :numbers))]
      (as-> (map
              (partial winning-board? numbers)
              (concat (get in :boards-rows) (get in :boards-cols))) boards
        (filter seq boards)
        (if (empty? boards)
          (recur (inc i))
          {:board   (first boards)
           :numbers numbers})))))

(defn solution [in winning-board-fnc]
  (let [wb                    (winning-board-fnc in)
        board                 (:board wb)
        numbers               (:numbers wb)
        number-set            (into #{} numbers)
        non-called-number-set (map #(set/difference % number-set) board)
        non-called-numbers    (map #(into [] %) non-called-number-set)
        last-number           (Integer. (last numbers))]
    (* (apply + (map #(Integer. %) (flatten non-called-numbers)))
       last-number)))

(defn part1 [in] (solution in get-winning-board))

(defn get-last-winning-board [in]
  (loop [i          1
         row-boards (get in :boards-rows)
         col-boards (get in :boards-cols)]
    (let [numbers (take i (get in :numbers))]
      (as-> (map
              (partial winning-board? numbers)
              (concat row-boards col-boards)) boards
        (filter seq boards)
        (if (empty? boards)
          (recur (inc i) row-boards col-boards)
          (let [idx-row (.indexOf row-boards (first boards))
                idx-col (.indexOf col-boards (first boards))
                idx     (if (= idx-row -1) idx-col idx-row)]
            (if (= (.size row-boards) 1)
              {:board   (first boards)
               :numbers numbers}
              (recur i
                (concat (take idx row-boards) (drop (+ idx 1) row-boards))
                (concat (take idx col-boards) (drop (+ idx 1) col-boards))))))))))

(defn part2 [in] (solution in get-last-winning-board))
