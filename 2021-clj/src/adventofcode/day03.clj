(ns adventofcode.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [in]
  (as-> (apply str ["day03/" in]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n")
    (map #(str/split % #"") f)
    (for [d f]
      (map #(Integer. %) d))))

(def inputdata (parse-input "input"))

(def testdata (parse-input "testdata"))

(defn max-key-default
  ([k d x] x)
  ([k d x y] (if (= (k x) (k y)) (clojure.lang.MapEntry. d (val x)) (if (> (k x) (k y)) x y)))
  ([k d x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (= (k x) (k y)) (clojure.lang.MapEntry. d kx) (if (> kx ky) [x kx] [y ky]))]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (= kw kv) (clojure.lang.MapEntry. d kw)
               (if (>= kw kv)
                 (recur w kw (next more))
                 (recur v kv (next more)))))
         v)))))

(defn min-key-default
  ([k d x] x)
  ([k d x y] (if (= (k x) (k y)) (clojure.lang.MapEntry. d (val x)) (if (< (k x) (k y)) x y)))
  ([k d x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (= (k x) (k y)) (clojure.lang.MapEntry. d kx) (if (< kx ky) [x kx] [y ky]))]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (= kw kv) (clojure.lang.MapEntry. d kw)
               (if (<= kw kv)
                 (recur w kw (next more))
                 (recur v kv (next more)))))
         v)))))


(defn get-gamma [in]
  (as-> (apply mapv vector in) v
    (map frequencies v)
    (map #(apply max-key-default val 1 %) v)
    (map key v)))

(defn get-epsilon [in]
  (as-> (apply mapv vector in) v
    (map frequencies v)
    (map #(apply min-key-default val 0 %) v)
    (map key v)))

(defn part1 [in]
  (let [gamma (Integer/parseInt (str/join (get-gamma in)) 2)
        epsilon (Integer/parseInt (str/join (get-epsilon in)) 2)]
    (* gamma epsilon)))

(defn get-rating [in fnc]
  (loop [data in
         idx 0]
    (if (= (.size data) 1)
      (first data)
      (let [f (fnc data)
            k (nth f idx)]
        (recur
         (filter #(= (nth % idx) k) data)
         (+ idx 1))))))

(defn get-oxygen-gen-rating [in]
  (get-rating in get-gamma))

(defn get-co2-scrubber-rating [in]
  (get-rating in get-epsilon))

(defn part2 [in]
  (let [gamma (Integer/parseInt (str/join (get-oxygen-gen-rating in)) 2)
        epsilon (Integer/parseInt (str/join (get-co2-scrubber-rating in)) 2)]
    (* gamma epsilon)))
