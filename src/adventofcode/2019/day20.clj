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
(def testdata3 (read-input "2019/day20/test3.txt"))

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
            (recur (uber/add-edges graph [node (vec edge) {:weight 1}]) (first edges) (rest edges)))))

(defn add-path [graph map position level]
    (let [x (first position) y (second position)
          above (get map [x (- y 1)])
          below (get map [x (+ y 1)])
          left (get map [(- x 1) y])
          right (get map [(+ x 1) y])]
        (let [position-with-level [x y level]
              edges (vec (partition 3 (concat
                        (if (= above ".") [x (- y 1) level] [])
                        (if (= below ".") [x (+ y 1) level] [])
                        (if (= left ".") [(- x 1) y level] [])
                        (if (= right ".") [(+ x 1) y level] []))))]
            (add-edges graph position-with-level edges))
    )
)

(defn is-portal? [tile]
    (and
        (not= tile "#")
        (not= tile " ")
        (not= tile ".")
        (not= tile nil)))

(defn build-initial-graph [i, graph, level]
    (let [map (convert-to-dict i)
          positions (for [y (range (height i)) x (range (width i))] [x y])]
        (loop [graph graph
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
                        (recur (add-path graph map current-pos level) (first positions) (rest positions))
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

(defn outer-edge? [edge height width]
    (let [x (first edge) y (second edge)]
        (if (or
            (< y 3)
            (< (- height y) 4)
            (< x 3)
            (< (- width x) 4))
            true
            false
            )))

(defn link-portals [graph portal-list height width]
    (let [portals (filter #(> (count (second %)) 1) (group-by first portal-list))]
        (loop [graph graph
                portal (first portals)
                portals portals]
            (if (nil? portal)
                graph
                (let [edges (take-nth 2 (drop 1 (apply concat (second portal))))]
                    (recur

                    ;; Graph
                    (if (outer-edge? (first edges) height width)
                        (uber/add-directed-edges
                            (uber/add-directed-edges graph [(first (uber/neighbors graph (second edges))) (first (uber/neighbors graph (first edges))) {:weight 1 :recursion -1}])
                            [(first (uber/neighbors graph (first edges))) (first (uber/neighbors graph (second edges))) {:weight 1 :recursion 1}])
                        (uber/add-directed-edges
                            (uber/add-directed-edges graph [(first (uber/neighbors graph (second edges))) (first (uber/neighbors graph (first edges))) {:weight 1 :recursion 1}])
                            [(first (uber/neighbors graph (first edges))) (first (uber/neighbors graph (second edges))) {:weight 1 :recursion -1}]))

                        (first portals)
                        (rest portals)))))))
;                    (recur (uber/add-edges graph [(first edges) (second edges) 0]) (first portals) (rest portals)))))))

(defn build-graph [i portals]
    (as-> (build-initial-graph i (uber/graph) 0) g
        (link-portals g portals (height i) (width i))))

(defn path-from-start-to-end [data]
    (let [portals (get-all-portals data)
        graph (build-graph data portals)
        start-pos (last (apply concat (filter #(= (first %) "AA") portals)))
        end-pos (last (apply concat (filter #(= (first %) "ZZ") portals)))]
        (println "Start:" start-pos)
        (println "End:" end-pos)
        (ualg/shortest-path graph start-pos end-pos :weight)
    ))

(defn part1 []
    (- (get (path-from-start-to-end inputdata) :cost) 2))

;P2

(defn link-portals-with-levels [graph level portal-list height width]
    (let [portals (filter #(> (count (second %)) 1) (group-by first portal-list))]
        ;(println level)
        (loop [graph graph
                portal (first portals)
                portals portals]
            (if (nil? portal)
                graph
                (let [edges (take-nth 2 (drop 1 (apply concat (second portal))))
                      [first-x first-y] (first edges)
                      [second-x second-y] (second edges)]
                    ;(println "level:" level "[" first-x first-y "] => [" second-x second-y "]")
                    (recur

                    ;; Graph
                    (if (outer-edge? (first edges) height width)
                        (uber/add-directed-edges
                            (uber/add-directed-edges graph [[first-x first-y level] [second-x second-y (- level 1)] {:weight 0}])
                            [[second-x second-y (- level 1)] [first-x first-y level] {:weight 0}])
                        (uber/add-directed-edges
                            (uber/add-directed-edges graph [[first-x first-y level] [second-x second-y (+ level 1)] {:weight 0}])
                            [[second-x second-y (+ level 1)] [first-x first-y level] {:weight 0}]))
                        ;)

                        (first portals)
                        (rest portals)))))
        ))


(defn build-graph-with-levels [i levels portals]
    (as-> (build-initial-graph i (uber/graph) 0) g
        ;(build-initial-graph i g -11)
        ;(build-initial-graph i g -10)
        ;(build-initial-graph i g -9)
        ;(build-initial-graph i g -8)
        ;(build-initial-graph i g -7)
        ;(build-initial-graph i g -6)
        ;(build-initial-graph i g -5)
        ;(build-initial-graph i g -4)
        ;(build-initial-graph i g -3)
        ;(build-initial-graph i g -2)
        ;(build-initial-graph i g -1)
        (build-initial-graph i g 1)
        (build-initial-graph i g 2)
        (build-initial-graph i g 3)
        (build-initial-graph i g 4)
        (build-initial-graph i g 5)
        (build-initial-graph i g 6)
        (build-initial-graph i g 7)
        (build-initial-graph i g 8)
        (build-initial-graph i g 9)
        (build-initial-graph i g 10)
        (build-initial-graph i g 12)
        ;(link-portals-with-levels g -10 portals (height i) (width i))
        ;(link-portals-with-levels g -9 portals (height i) (width i))
        ;(link-portals-with-levels g -8 portals (height i) (width i))
        ;(link-portals-with-levels g -7 portals (height i) (width i))
        ;(link-portals-with-levels g -6 portals (height i) (width i))
        ;(link-portals-with-levels g -5 portals (height i) (width i))
        ;(link-portals-with-levels g -4 portals (height i) (width i))
        ;(link-portals-with-levels g -3 portals (height i) (width i))
        ;(link-portals-with-levels g -2 portals (height i) (width i))
        ;(link-portals-with-levels g -1 portals (height i) (width i))
        (link-portals-with-levels g 0 portals (height i) (width i))
        (link-portals-with-levels g 1 portals (height i) (width i))
        (link-portals-with-levels g 2 portals (height i) (width i))
        (link-portals-with-levels g 3 portals (height i) (width i))
        (link-portals-with-levels g 4 portals (height i) (width i))
        (link-portals-with-levels g 5 portals (height i) (width i))
        (link-portals-with-levels g 6 portals (height i) (width i))
        (link-portals-with-levels g 7 portals (height i) (width i))
        (link-portals-with-levels g 8 portals (height i) (width i))
        (link-portals-with-levels g 9 portals (height i) (width i))
        (link-portals-with-levels g 10 portals (height i) (width i))
        (link-portals-with-levels g 11 portals (height i) (width i))
        ))


(defn shortest-path-at-level-zero-recur [graph
        node
        end-pos
        visited
        recursion
        cumulative-dist
        steps]
    (if (> steps 1000)
        nil ; {:finished false :dist cumulative-dist :recursion recursion}
        (if (get visited [(first node) (second node) recursion])
            nil ; {:dist cumulative-dist :recursion recursion :finished false}
            (if (and (= node end-pos) (= recursion 0))
                {:dist cumulative-dist :recursion recursion}
                (for [out-edge (uber/out-edges graph node)]
                        (let [out-edge-attrs (uber/attrs graph out-edge)
                              neighbor (get out-edge :dest)]

                        (if (= node end-pos)
                            (println "Found end, recursion:" recursion ", dist:" cumulative-dist ", steps:" steps))

                        (shortest-path-at-level-zero-recur
                                graph
                                neighbor
                                end-pos
                                (assoc visited [(first node) (second node) recursion] 1)
                                (+ recursion (get out-edge-attrs :recursion))
                                (+ cumulative-dist (get out-edge-attrs :weight))
                                (+ steps 1)
                        ))
                ))
            )))

(defn shortest-path-at-level-zero [graph node end-pos]
    (shortest-path-at-level-zero-recur graph node end-pos {} 0 0 0))

(defn path-from-start-to-end-level-zero [data]
    (let [portals (get-all-portals data)
        graph (build-graph-with-levels data 8 portals)
        start-pos (last (apply concat (filter #(= (first %) "AA") portals)))
        end-pos (last (apply concat (filter #(= (first %) "ZZ") portals)))]
        ;(uber/pprint graph)
        (println "Start:" start-pos)
        (println "End:" end-pos)
        ;(ualg/edges-in-path
        (let [path (ualg/shortest-path graph {
                :start-node (concat start-pos [4])
                :end-node (concat end-pos [4])
                :cost-fn (fn [e] (+ (uber/attr graph e :weight)))
                ;:traverse true
            })
            edges-in-path (ualg/edges-in-path path)
            edges-with-attrs (map #(uber/edge-with-attrs graph %) edges-in-path)
            ]
            edges-with-attrs
            ;(ualg/nodes-in-path (last path))
            )
        ;graph
        ;)
        ))

(defn part2 []
    (path-from-start-to-end-level-zero inputdata))
