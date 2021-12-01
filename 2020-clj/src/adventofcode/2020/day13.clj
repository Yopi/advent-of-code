(ns adventofcode.2020.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file fnc]
  (as-> (apply str ["2020/day13/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map-indexed fnc f)))

(defn p1-map [idx x]
  (if (= idx 0)
    (Long. x)
    (map #(Long. %) (filter #(not= "x" %) (str/split x #",")))))

(def testdata (input "test1.txt" p1-map))
(def inputdata (input "input.txt" p1-map))

(defn part1 [in]
  (let [current-time (first in)
        schedule (second in)
        time-until-bus (map (fn [x] {:bus-id x :time-until (- x (mod current-time x))}) schedule)
        earliest-bus (apply min-key #(get % :time-until) time-until-bus)]
    (println current-time)
    (println earliest-bus)
    (* (get earliest-bus :bus-id)
       (get earliest-bus :time-until))))

(part1 inputdata)

(defn p2-map [idx x]
  (if (= idx 0)
    (Long. x)
    (as-> (str/split x #",") s
      (map-indexed (fn [i n]
                     (cond 
                       (= i 0) [0 (Long. n)]
                       (not= n "x") [(- (Long. n) i) (Long. n)]
                       :else nil))  s)
      (filter some? s))))


(def testdata-p2 (second (input "test1.txt" p2-map)))
(def inputdata-p2 (second (input "input.txt" p2-map)))
(def testdata2-p2 (second (map-indexed p2-map ["0" "17,x,13,19"])))
(def testdata3-p2 (second (map-indexed p2-map ["0" "67,7,59,61"])))
(def testdata4-p2 (second (map-indexed p2-map ["0" "67,x,7,59,61"])))

; x = 0 % 17
; x = 11 % 13
; x = 16 % 19
; 3417

(defn extended-gcd [a b]
  (cond (zero? a) [(Math/abs b) [0 1]]
        (zero? b) [(Math/abs a) [1 0]]
        :else
        (loop [old_r a
               r b
               old_s 1
               s 0
               old_t 0
               t 1]
          (if (= r 0)
            [(Math/abs old_r) [old_s old_t]]
            (let [quotient (quot old_r r)]
              (recur r (- old_r (* quotient r))
                     s (- old_s (* quotient s))
                     t (- old_t (* quotient t))))))))

(defn inverse-mod [a n]
  (let [[g [x y]] (extended-gcd a n)]
    (if (not= g 1)
      (throw (Exception. "Cannot compute"))
      (mod x n))))

(defn chinese-remainders [nums]
  (let [N (apply *' (map second nums))
        s (map (fn [[n m]]
                 (/ (*' (*' n N) (inverse-mod (quot N m) m)) m)) nums)]
    (mod (reduce + s) N)))

(defn part2 [in]
  (chinese-remainders in))

(part2 inputdata-p2)

(comment (extended-gcd 451763 7)
         (chinese-remainders testdata-p2))
