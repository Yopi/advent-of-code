(ns adventofcode.2019.day20
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [ubergraph.core :as uber]
        [ubergraph.alg :as ualg]))

(defn read-input [in]
    (as-> (clojure.java.io/resource in) i
        (slurp i)
        (str/split i #"\n")
        (map #(str/split % #"") i)
    ))

(def inputdata (read-input "2019/day20/input.txt"))

(def testdata (read-input "2019/day20/test1.txt"))
(def testdata2 (read-input "2019/day20/test2.txt"))

(defn width [i]
    (apply max (map count i)))

(defn height [i]
    (count i))

(defn convert-to-dict [i]
    (into {} (for [y (range (height i))
            x (range (width i))]
        {[x y] (nth (nth i y " ") x " ")})))

(defn add-edges [graph node edges]
    (loop [graph graph
            edge (first edges)
            edges (rest edges)]
        (if (nil? edge)
            graph
            (recur (uber/add-edges graph [node (vec edge) 1]) (first edges) (rest edges)))))

(defn add-path [graph map position]
    (let [x (first position) y (second position)
          above (get map [x (- y 1)])
          below (get map [x (+ y 1)])
          left (get map [(- x 1) y])
          right (get map [(+ x 1) y])]
        (let [edges (vec (partition 2 (concat
                        (if (= above ".") [x (- y 1)] [])
                        (if (= below ".") [x (+ y 1)] [])
                        (if (= left ".") [(- x 1) y] [])
                        (if (= right ".") [(+ x 1) y] []))))]
            (add-edges graph position edges))
    )
)

(defn is-portal? [tile]
    (and
        (not= tile "#")
        (not= tile " ")
        (not= tile ".")
        (not= tile nil)))

(defn build-initial-graph [i]
    (let [map (convert-to-dict i)
          positions (for [y (range (height i)) x (range (width i))] [x y])]
        (loop [graph (uber/graph)
               current-pos (first positions)
               positions (rest positions)]
            (if (nil? current-pos)
                graph
                (let [x (first current-pos)
                      y (second current-pos)
                      current-tile (get map [x y])]
                    (if (and
                            (not= current-tile "#")
                            (not= current-tile " "))
                        (recur (add-path graph map current-pos) (first positions) (rest positions))
                        (recur graph (first positions) (rest positions)))
                )))))

(defn incoming-coordinate [m c1 c2]
    (if (or
        (= (get m [(- (first c1) 1) (second c1)]) ".")
        (= (get m [(+ (first c1) 1) (second c1)]) ".")
        (= (get m [(first c1) (- (second c1) 1)]) ".")
        (= (get m [(first c1) (+ (second c1) 1)]) "."))
        c1
        c2))


(defn get-all-portals [i]
    (filter some?
        (let [m (convert-to-dict i)]
            (for [y (range (height i)) x (range (width i))]
                (let [current-tile (get m [x y])
                        right (get m [(+ x 1) y])
                        down (get m [x (+ y 1)] )]
                    (if (is-portal? current-tile)
                        (if (is-portal? right)
                            [(str current-tile right) (incoming-coordinate m [x y] [(+ x 1) y])] ;;;; [x y] måste vara koordinaten närmast .
                            (if (is-portal? down)
                                [(str current-tile down) (incoming-coordinate m [x y] [x (+ y 1)])]))))))))

(defn link-portals [graph portal-list]
    (let [portals (filter #(> (count (second %)) 1) (group-by first portal-list))]
        (loop [graph graph
                portal (first portals)
                portals portals]
            (if (nil? portal)
                graph
                (let [edges (take-nth 2 (drop 1 (apply concat (second portal))))]
                    (recur (uber/add-edges graph [
                            (first (uber/neighbors graph (first edges)))
                            (first (uber/neighbors graph (second edges))) 1])
                        (first portals)
                        (rest portals)))))))

;                    (recur (uber/add-edges graph [(first edges) (second edges) 0]) (first portals) (rest portals)))))))

(defn build-graph [i portals]
    (as-> (build-initial-graph i) g
        (link-portals g portals)))


(defn path-from-start-to-end [data]
    (let [portals (get-all-portals data)
        graph (build-graph data portals)
        start-pos (last (apply concat (filter #(= (first %) "AA") portals)))
        end-pos (last (apply concat (filter #(= (first %) "ZZ") portals)))]
        ;(uber/pprint graph)
        (println "Start:" start-pos)
        (println "End:" end-pos)
        ;(ualg/edges-in-path
            (ualg/shortest-path graph start-pos end-pos :weight)
        ;)
        ))

(defn part1 []
    (- (get (path-from-start-to-end inputdata) :cost) 2))
