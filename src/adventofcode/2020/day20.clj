(ns adventofcode.2020.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as m]))

(defn to-matrix [in]
  (as-> (str/split in #"\n") rows
    (map #(str/split % #"") rows)
    (for [y (range (count rows))]
      (for [x (range (count (first rows)))]
        (nth (nth rows y) x)))))

(defn transpose [xs]
  (apply map list xs))

(defn flip [xs]
  (map reverse xs))

(defn rotate-left [xs]
  (transpose (flip xs)))

(defn input [file]
  (as-> (apply str ["2020/day20/" file]) f
    (clojure.java.io/resource f)
    (clojure.java.io/file f)
    (slurp f)
    (str/split f #"\n\n")
    (map (fn [x]
           (let [l (str/split x #"\n")]
             {:tile (Integer. (first (re-seq #"\d+" (first l))))
              :picture (to-matrix (str/join "\n" (rest l)))})) f)))

(def inputdata (input "input.txt"))
(def testdata (input "test1.txt"))

(defn side-matches? [side p1 p2]
  (cond
    (= side "L") (= (first (m/columns p1)) (last (m/columns p2)))
    (= side "R") (= (last (m/columns p1)) (first (m/columns p2)))
    (= side "U") (= (first p1) (last p2))
    (= side "D") (= (last p1) (first p2))))

(defn all-rotations [piece]
  [piece
   (rotate-left piece)
   (rotate-left (rotate-left piece))
   (rotate-left (rotate-left (rotate-left piece)))
   (flip piece)
   (rotate-left (flip piece))
   (rotate-left (rotate-left (flip piece)))
   (rotate-left (rotate-left (rotate-left (flip piece))))])

(defn match-piece-against-all-sides [p1 p2]
  (let [all-rots (all-rotations p2)]
    [(not (empty? (filter true? (map (partial side-matches? "L" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "R" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "U" p1) all-rots))))
     (not (empty? (filter true? (map (partial side-matches? "D" p1) all-rots))))]))

(defn get-matches [in p1]
  (map (fn [x]
         (match-piece-against-all-sides p1 x)) (map #(get % :picture) in)))

(defn part1 [in]
  (reduce
   *
   (loop [piece (first in)
          pieces (rest in)
          corners []]
     (if (nil? piece)
       corners
       (let [matches (get-matches in (get piece :picture))
             matching-corners (map (fn [x] (reduce #(or %1 %2) x)) matches)
             num-matches (count (filter true? matching-corners))]
         num-matches
         (if (= num-matches 3)
           (recur (first pieces) (rest pieces) (conj corners (get piece :tile)))
           (recur (first pieces) (rest pieces) corners)))))))

; (part1 inputdata)

(defn rotation-matches? [side p1 p2]
  (cond
    (= side "L") (if (= (first (m/columns p1)) (last (m/columns p2))) p2)
    (= side "R") (if (= (last (m/columns p1)) (first (m/columns p2))) p2)
    (= side "U") (if (= (first p1) (last p2)) p2)
    (= side "D") (if (= (last p1) (first p2)) p2)))

(defn find-matching-rotations [p1 p2s]
  (let [p2 (get p2s :pictures)
        tile (get p2s :tile)
    matches (into {} (filter (comp not-empty val)
                      {:L (apply concat (filter not-empty (map (partial rotation-matches? "L" p1) p2)))
                       :R (apply concat (filter not-empty (map (partial rotation-matches? "R" p1) p2)))
                       :U (apply concat (filter not-empty (map (partial rotation-matches? "U" p1) p2)))
                       :D (apply concat (filter not-empty (map (partial rotation-matches? "D" p1) p2)))}))]
    (if (empty? matches)
      {}
      (assoc matches :tile tile))))

(defn reverse-map
  [m]
  (into {} (map (fn [[k v]] [(get v :tile) k]) m)))

(defn filter-out-tiles [tiles seen-tiles]
  (let [seen (into #{} (map #(get % :tile) seen-tiles))]
    (filter
     (fn [x]
       (not (contains? seen (get x :tile))))
     tiles
     )))

(defn find-matches [in]
  (loop [piece (first in)
         rest-pieces []
         m {[0 0] piece}]
    (let [coord (get (reverse-map m) (get piece :tile))]
      (cond
        (nil? piece) m
        :else (let [[x y] coord
                    pieces-with-rotations (map (fn [x] {:tile (get x :tile), :pictures (all-rotations (get x :picture))}) (filter-out-tiles in (vals m)) )
                    all-matches (map (partial find-matching-rotations (get piece :picture)) pieces-with-rotations)
                    new-m (into {}
                                (for [match all-matches]
                                  (let [tile (get match :tile)
                                        left (get match :L)
                                        right (get match :R)
                                        up (get match :U)
                                        down (get match :D)]
                                    (cond
                                      (not-empty left) (assoc {} [(- x 1) y] {:tile tile, :picture left})
                                      (not-empty right) (assoc {} [(+ x 1) y] {:tile tile, :picture right})
                                      (not-empty up) (assoc {} [x (- y 1)] {:tile tile, :picture up})
                                      (not-empty down) (assoc {} [x (+ y 1)] {:tile tile, :picture down})
                                      :else {}))))
                    new-rest (concat rest-pieces (filter-out-tiles (vals new-m) (vals m)))
                    ]
                (recur
                 (first new-rest)
                 (rest new-rest)
                 (merge m new-m)
                 ))))))

(defn cut-image [img]
  (as-> (drop 1 img) i
    (drop-last i)
    (map #(drop-last (drop 1 %)) i)
    (map str/join i)
    ))

(defn compute-picture [matches]
  (let [coords (keys matches)
        xs (map first coords)
        ys (map second coords)]
    (map #(str/split % #"")
         (apply concat
                (for [y (range (apply min ys) (inc (apply max ys)))]
                  (apply map str (for [x (range (apply min xs) (inc (apply max xs)))]
                                   (cut-image (get (get matches [x y]) :picture)))))))))

(defn join-picture [picture]
  (str/join "\n"
            (map #(str/join "\n" %) picture)))

(def sea-monster
  [#"..................#."
   #"#....##....##....###"
   #".#..#..#..#..#..#..."])

(def sea-monster-stats
  {:count
   (get (frequencies (str/join (map str sea-monster))) \#)})

(defn find-sea-monsters [picture]
  (loop [a (first picture)
         rows (rest picture)
         matches 0]
    (if (< (count rows) 2)
      matches
      (let [b (nth rows 0)
            c (nth rows 1)
            found-monsters (min
                     (count (re-seq (nth sea-monster 0) a))
                     (count (re-seq (nth sea-monster 1) b))
                     (count (re-seq (nth sea-monster 2) c))
                     )]
        (if (> found-monsters 0)
            (recur
             (first rows)
             (rest rows)
             (+ matches found-monsters))
          (recur
           (first rows)
           (rest rows)
           matches))))))

(defn count-monsters-and-sea [picture]
  (let [monsters (find-sea-monsters picture)
        sea (get (frequencies (str/join picture)) \#)]
    {:sea sea
     :monsters monsters}))

(defn get-sea-count [picture]
  (let [ms (count-monsters-and-sea picture)]
    (- (get ms :sea)
       (* (get ms :monsters) (get sea-monster-stats :count)))))

(defn part2 [in]
  (->> in
       (find-matches)
       (compute-picture)
       (all-rotations)
       (map (fn [rotation] ; one rotation
              (map str/join rotation)))
       (map get-sea-count)
       (apply min)))

(part2 inputdata)
