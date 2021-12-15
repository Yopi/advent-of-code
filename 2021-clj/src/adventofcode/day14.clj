(ns adventofcode.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defn parse-input [in]
  (as-> (apply str ["day14/" in]) f
    (clojure.java.io/resource f)
    (slurp f)
    (str/trim-newline f)
    (str/split f #"\n\n")
    (let [[template insertions] f]
      {:template template
       :insertions (apply merge (map (fn [x] (let [[_ pair polymer] (first (re-seq #"([A-Z]{2}) -> ([A-Z])" x))]
                                  {pair polymer})) (str/split-lines insertions)))
      })))

(def inputdata (parse-input "input"))
(def testdata (parse-input "testdata"))

; Naive solution P1
(defn step-fnc [in]
  (let [{:keys [template insertions]} in
        pairs                         (partition 2 1 template)
        polymer                       (map (fn [[a b]]
                                             [a (get insertions (str a b)) b]) pairs)]
    {:template   (apply str (flatten
                              (map-indexed (fn [idx itm]
                                             (if (odd? idx)
                                               (if (= idx (- (count polymer) 1))
                                                 (drop 1 itm)
                                                 (take 1 (drop 1 itm)))
                                               itm)) polymer)))
     :insertions insertions}))

(defn steps [in max-steps]
  (let [{:keys [template insertions]} (loop [in   in
                                             step 0]
                                        (println (get in :template))
                                        (if (= step max-steps)
                                          in
                                          (recur (step-fnc in) (inc step))))]
    template))

(defn part1 [in]
  (let [template (steps in 10)
        sorted   (map second
                   (sort-by val > (frequencies template)))]
    (- (first sorted) (last sorted))))

; Smarter solution P2
(defn step-fnc-2 [template insertions]
  (apply merge-with +
    (map (fn [[[a b] v]]
           (let [new (get insertions (str a b))]
             (merge-with + {[a new] v
                            [new b] v}))) template)))

(defn steps-2 [in max-steps]
  (let [{:keys [template insertions]} in
        fixed-template                (apply merge (map (fn [x]
                                                          {(vec (map str x)) 1}) (partition 2 1 template)))]
    (loop [fixed-template fixed-template
           step           0]
      (if (= step max-steps)
        fixed-template
        (recur (step-fnc-2 fixed-template insertions) (inc step))))))

(defn part2 [in]
  (let [template       (steps-2 in 40)
        template-parts (apply merge-with + (map (fn [[[_a b] c]] {b c}) template))
        sorted         (map second (sort-by val > template-parts))]
    (- (first sorted) (last sorted))))
