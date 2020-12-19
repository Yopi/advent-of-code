(ns adventofcode.2020.day19
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [instaparse.core :as insta]))

(defn parse-rules [in]
  (as-> (str/split in #"\n") i
    (sort i)
    (str/join "\n" i)
    (insta/parser i)))

(defn input [file]
  (as-> (apply str ["2020/day19/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n")
    {:rules (first f)
     :input (str/split (second f) #"\n")}))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(defn part1 [in]
  (let [rules (parse-rules (get in :rules))
        input (get in :input)]
    (as-> (map #(insta/parses rules %) input) parsed
      (filter #(false? (insta/failure? %)) parsed)
      (count parsed))))

(defn part2[in]
  (let [rules-str (get in :rules)
        fixed-rules (str/replace (str/replace
                      rules-str
                      "8: 42" "8: 42 | 42 8")
                     "11: 42 31"
                     "11: 42 31 | 42 11 31")
        rules (parse-rules fixed-rules)
        input (get in :input)]
    (as-> (map #(insta/parses rules %) input) parsed
      (filter #(false? (insta/failure? %)) parsed)
      (count parsed))))

(part1 inputdata)
(part2 inputdata)
