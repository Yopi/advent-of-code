(ns adventofcode.2019.day14
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]))

(defn to-chem-and-quantity [s]
    (let [[quantity chemical] (str/split s #" ")]
        {chemical (Integer. quantity)}))

(defn to-reaction [r]
    (let [lh (str/split (first r) #", ")
            rh (str/split (second r) #", ")]
        [(into {} (map to-chem-and-quantity lh)) (into {} (map to-chem-and-quantity rh))]
        ))

(defn parse-input [file]
    (as-> (slurp (clojure.java.io/file (clojure.java.io/resource  file))) in
        (str/split in #"\n")
        (map #(str/split % #" => ") in)
        (map to-reaction in)))

(def test1 (parse-input "2019/day14/test1.txt"))
(def test2 (parse-input "2019/day14/test2.txt"))
(def test3 (parse-input "2019/day14/test3.txt"))
(def test4 (parse-input "2019/day14/test4.txt"))
(def test5 (parse-input "2019/day14/test5.txt"))
(def input (parse-input "2019/day14/input.txt"))

(defn remove-from-quantities [quantities reaction]
    (into quantities 
        (for [subject (keys reaction)]
            [subject (- (get quantities subject 0) (get reaction subject))])))

(defn add-to-quantities [quantities reaction]
    (into quantities
        (for [[subject, quantity] reaction]
            [subject (+ (get quantities subject 0) (get reaction subject))])))

(defn calculate-reaction [quantities lh rh]
    (let [requirements (keys lh)
            has-requirements? (for [requirement requirements]
                                (if (>= (get quantities requirement 0) (get lh requirement))
                                    true
                                    false))]
            ;(println "Req:" requirements ", ? " has-requirements?)
            (if (every? true? has-requirements?)
                {:removes lh, :produces rh}
                nil)
    )
)

(defn make-reaction [quantities reactions]
    (let [reacts reactions]
    (loop [quantities quantities
            r (first reacts)
            other (rest reacts)] 
        (do
        (if (nil? r)
            quantities
            (recur 
                (calculate-reaction quantities (first r) (second r)) 
                (first other)
                (rest other)))))))

(defn get-fuel [initial-ore, reactions]
    (let [quantities {"ORE" initial-ore}]
        (loop [quantities quantities
                step 0]
            (let [new-q (make-reaction quantities reactions)]
                new-q))))

;(defn calculate-resources-needed [reactions looking-for]
;    (into {} (filter some? 
;        (for [r reactions
;                [l v] looking-for]
;        (let [result (first (first (second r)))
;                result-value (second (first (second r)))
;                required (first r)
;                required-value (second (first required))
;                to-be-left (Math/floor (/ v required-value))]
;            (println required)
;            (println to-be-left)
;            (if (= result l)
;                {
;                    (first (first required))
;                    (* to-be-left result-value)
;                    l
;                    (- v (* to-be-left result-value))
;                }
;        ))))))

(defn calculate-resources-needed [reactions looking-for]
    (for [r reactions
            [looking-chemical looking-count] looking-for]
        (let [found-reaction (first (first (second r)))
                found-reaction-count (second (first (second r)))
                ]
            (println found-reaction)
            (println found-reaction-count)
            )))


; (defn calculate-resources-needed [reactions looking-for]
;     (loop [results {}
;             looking-for looking-for
;             step 0]
;         (let [additional-resources (into {} 
;             (filter some? 
;                 (for [r reactions
;                         [l v] looking-for]
;                 (let [result (first (first (second r)))
;                         result-quantity (second (first (second r)))
;                         required (first r)]
;                     (println "Result: " result ", l " l)
;                     (println (first (second r)))
;                     (if (and (= result l) (>= result-quantity v))
;                         required
;                         nil
;                 )))))]
; 
;             (let [resource-count (count (keys additional-resources))
;                     first-resource (first (keys additional-resources))]
;                 (if (and (= first-resource "ORE") (= resource-count 1))
;                     (merge-with + results additional-resources)
;                     (recur (merge-with + results additional-resources) additional-resources (+ step 1)))
;         ))))



(defn bruteforce-get-fuel [reactions]
    (let [initial-ore 100000]
    (loop [runs []
            retries 0]
        (let [run (get-fuel initial-ore reactions)
                subjects (filter #(> (second %) 0) (first run))
                ore-required (- initial-ore (get (first run) "ORE"))
                met-ore-requirements (if (>= (get (first run) "FUEL" 0) 1) ore-required initial-ore)]
            ;(println (first run))
            (if (< retries 100)
                (recur (concat [met-ore-requirements] runs) (+ retries 1))
                (flatten (conj [met-ore-requirements] runs)))))))

(defn x [reactions] 

    )