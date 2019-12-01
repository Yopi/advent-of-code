(ns adventofcode.2018.day04
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(def inputdata
    (str/split
      (slurp (io/file (io/resource  "day04/sorted_input.txt")))
      #"\n"))

(def testdata
    (str/split
      (slurp (io/file (io/resource  "day04/sorted_example.txt")))
      #"\n"))

(defn solutions [guard-sleeping]
    ; Get the guard that sleeps the most
    (def part1-guard
        (key (apply max-key val (apply merge (for [[g times] guard-sleeping]
            (hash-map g (reduce + (flatten (map (fn [ts] (range (second ts) (first ts))) times)))))))))

    ; Get the day on which the guards were sleeping the most
    (def part1-day
        (let [times (get guard-sleeping part1-guard)]
            (let [freq (frequencies (flatten (map (fn [ts] (range (second ts) (first ts))) times)))]
                (let [maxk (key (apply max-key val freq))]
                    [maxk (get freq maxk)]))))

    ; Which guard spends the same minute the sleep the most
    (def part2
        (last (sort-by #(nth % 2) (for [[g times] guard-sleeping]
        (let [freq (frequencies (flatten (map (fn [ts] (range (second ts) (first ts))) times)))]
            (let [maxk (key (apply max-key val freq))]
                [g maxk (get freq maxk)])))))))

(defn set-sleeping [guard, t]
    (def guard-sleeping (assoc guard-sleeping
        guard
        (conj (get guard-sleeping guard) t))))

(defn set-awake [guard, t]
    (def guard-sleeping (assoc guard-sleeping
        guard
        (conj (get guard-sleeping guard) t))))

(defn assign-guard-schedule [input]
    (do
        (def guard-sleeping {})
        (loop [r input
                guard 0]
            (if (= (count r) 0)
                nil
                (if-let [[a g] (re-find #".*Guard #(\d+) begins shift" (first r))]
                    (recur (rest r) g)
                    (if-let [[a t1] (re-find #".*(\d\d)\] falls asleep" (first r))]
                        (do
                            (set-sleeping guard (Integer. t1))
                            (recur (rest r) guard))
                        (if-let [[a t2] (re-find #".*(\d\d)\] wakes up" (first r))]
                            (do
                                (set-awake guard (Integer. t2))
                                (recur (rest r) guard))
                            (recur (rest r) guard))))))
        (for [[g times] guard-sleeping]
            (def guard-sleeping
            (assoc guard-sleeping
                g
                (partition 2 times))))
        ; (solutions guard-sleeping) ;; Doesn't work to run this as part of this defn, why?
        ))
