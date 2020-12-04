(ns adventofcode.2020.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input [file]
  (map #(str/split % #"\n| ")
       (str/split
        (slurp (clojure.java.io/file (clojure.java.io/resource  (apply str ["2020/day04/" file]))))
        #"\n\n")))

(defn parse-input [data]
  (for [pass data]
    (apply hash-map (flatten (map #(str/split % #":") pass)))))

(def testdata (parse-input (input "test1.txt")))
(def testdata2 (parse-input (input "test2.txt")))
(def testdata3 (parse-input (input "test3.txt")))
(def inputdata (parse-input (input "input.txt")))

; All fields
;;    byr (Birth Year) - four digits; at least 1920 and at most 2002.
;;    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;;    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;;    hgt (Height) - a number followed by either cm or in:
;;        If cm, the number must be at least 150 and at most 193.
;;        If in, the number must be at least 59 and at most 76.
;;    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;;    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;;    pid (Passport ID) - a nine-digit number, including leading zeroes.
;;    cid (Country ID) - ignored, missing or not.

(defn passport-contains-keys? [pass]
  (cond-> []
    (contains? pass "byr") (conj 1)
    (contains? pass "iyr") (conj 1)
    (contains? pass "eyr") (conj 1)
    (contains? pass "hgt") (conj 1)
    (contains? pass "hcl") (conj 1)
    (contains? pass "ecl") (conj 1)
    (contains? pass "pid") (conj 1)))

(defn valid-number? [yr min max]
  (and 
   (some? yr)
   (and
    (>= (Integer. yr) min)
    (<= (Integer. yr) max))))

(defn valid-height? [hgt]
  (and
   (some? hgt)
   (and
    (re-matches #"\d+(in|cm)" hgt)
    (if (re-matches #"\d+cm" hgt)
      (valid-number? (subs hgt 0 (- (count hgt) 2)) 150 193)
      (valid-number? (subs hgt 0 (- (count hgt) 2)) 59 76)))))

(defn valid-hair-color? [hcl]
  (and
   (some? hcl)
   (re-matches #"#[0-9a-f]{6}" hcl)))

(defn valid-eye-color? [ecl]
  (and
   (some? ecl)
   (re-matches #"amb|blu|brn|gry|grn|hzl|oth" ecl)))

(defn valid-pid? [pid]
  (and
   (some? pid)
   (re-matches #"\d{9}" pid)))

(defn passport-valid-keys? [pass]
  (cond-> []
    (valid-number? (get pass "byr") 1920 2002) (conj 1)
    (valid-number? (get pass "iyr") 2010 2020) (conj 1)
    (valid-number? (get pass "eyr") 2020 2030) (conj 1)
    (valid-height? (get pass "hgt")) (conj 1)
    (valid-hair-color? (get pass "hcl")) (conj 1)
    (valid-eye-color? (get pass "ecl")) (conj 1)
    (valid-pid? (get pass "pid")) (conj 1)    
    ))

(defn part1 [passports]
  (count
          (filter #(>= % 7)
                  (map #(apply + %)
                       (map passport-contains-keys? passports)))))

(defn part2 [passports]
  (count
   (filter #(>= % 7)
           (map #(apply + %)
                (map passport-valid-keys? passports)))))

(comment
  testdata
  (contains? (first testdata) "pidx")
  (passport-contains-keys? (first testdata))
  (part1 testdata)
  (re-matches #"\d+(in|cm)" "196ix")
  (valid-height? "194cm")
  (part1 inputdata)
  (part2 inputdata)
  (valid-height? nil)
  (passport-valid-keys? (nth testdata 1))
  )


