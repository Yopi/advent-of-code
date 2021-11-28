(ns adventofcode.2019.day18
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]
        [clojure.set :as set]
        [ubergraph.core :as uber]
        [ubergraph.alg :as alg]))

(defn parse-map [input]
    (into {}
        (filter some?
            (flatten
                (for [y (range (count input))]
                    (for [x (range (count (nth input y)))]
                        (let [tile (nth (nth input y) x)]
                            (if (not= tile "#")
                                {[x y] tile}
                                nil)
                    )))))))

(defn read-input [f]
    (as-> (clojure.java.io/resource f) i
        (clojure.java.io/file i)
        (slurp i)
        (str/split i #"\n")
        (map #(str/split % #"") i)
        (parse-map i)))

(def inputdata (read-input "2019/day18/input.txt"))

(def test1 (read-input "2019/day18/test1.txt"))
(def test2 (read-input "2019/day18/test2.txt"))


;(defn nodes-in-input [input]
;    (->> input
;        (map #(str/split % #"\)") ,,,)
;        (flatten ,,,)
;        (set ,,,)
;        (seq ,,,)))
;
;(def test-graph-directed
;    (apply uber/add-directed-edges (uber/graph (first (nodes-in-input testdata)))
;        (for [orbit testdata] (str/split orbit #"\)"))))
;
;(def input-graph-directed
;    (apply uber/add-directed-edges (uber/graph (first (nodes-in-input inputdata)))
;        (for [orbit inputdata] (str/split orbit #"\)"))))
;
;
;(defn walk-graph [graph root]
;    (let [orbiters (uber/predecessors graph root)]
;        (if (empty? orbiters)
;            '(0)
;            (for [orbiter orbiters]
;                (concat '(1) (walk-graph graph orbiter))))))
;
;
;(defn part1 [graph]
;    (reduce + (flatten
;        (for [n (uber/nodes graph)] (walk-graph graph n)))))


(defn start-position [input]
    (first (filter (comp #{"@"} input) (keys input))))

(defn neighbors [karta position]
    (filter some?
    [
        (if (some? (get karta [(- (first position) 1) (second position)])) [(- (first position) 1) (second position)] nil)
        (if (some? (get karta [(+ (first position) 1) (second position)])) [(+ (first position) 1) (second position)] nil)
        (if (some? (get karta [(first position) (- (second position) 1)])) [(first position) (- (second position) 1)] nil)
        (if (some? (get karta [(first position) (+ (second position) 1)])) [(first position) (+ (second position) 1)] nil)
    ])
)

(defn is-locked? [in]
    (every? #(Character/isUpperCase %) in))

(defn is-item? [in]
    (and (not= "." in)
         (not= "@" in)))

(defn position-of-unlocks [in position]
    (let [key (get in position)]
        (println position ":" key)
        (get (clojure.set/map-invert in) (str/upper-case key))))

(defn total-keys [in]
    (count (filter #(re-matches #"[a-z]" %) (vals in))))

(defn build-initial-graph [karta]
    (let [graph (uber/graph)
            start-pos (start-position karta)]
        (loop [node start-pos
                nodes []
                visited []
                graph graph
                iterations 0]
            (if (or (nil? node) (> iterations 1000))
                graph
                (let [all-ns (neighbors karta node)
                        new-ns (remove #(.contains visited %) (concat all-ns nodes))]
                    (if (empty? new-ns) ; Exhausted all possible neighbors
                        graph
                        (recur (first new-ns) (rest new-ns) (conj visited node) (uber/add-edges* graph (map #(conj [node] %) (neighbors karta node))) (inc iterations))
                    ))))))

(defn enrich-graph [graph karta]
    (loop [g graph
           node (first (uber/nodes g))
           nodes (rest (uber/nodes g))]
       (if (nil? node)
           g
           (let [g-with-n (uber/set-attrs g node {:val (get karta node) :item (is-item? (get karta node)) :locked (is-locked? (get karta node))})]
               (recur g-with-n
                      (first nodes)
                      (rest nodes))))))

(defn build-graph [in]
    (as-> in karta
        (build-initial-graph karta)
        (enrich-graph karta in)))

(defn closest-key [g start-pos]
    (alg/shortest-path g
        {
            :start-node start-pos,
            :end-node? (fn [n] (= true (uber/attr g n :item))),
            :node-filter (fn [n] (= false (uber/attr g n :locked)))
            :traverse true
        }))

(defn remove-key-and-door [in g path]
    (let [door-pos (position-of-unlocks in (get path :end))]
        (if (some? door-pos)
            (uber/set-attrs
                ; Remove door
                (uber/set-attrs g door-pos {:val "." :item false :locked false})
                ; Remove key
                (get path :end) {:val "@" :item false :locked false})
            (uber/set-attrs
                g ; There is no door, just remove key
                (get path :end) {:val "@" :item false :locked false}))))

(defn cost-to-all-keys [in]
    (let [graph (build-graph in)
            total-keys (total-keys in)]
        (loop [g graph
                from-node (start-position in)
                keys 0
                cost 0]
            (if (= keys total-keys)
                cost
                (let [path (closest-key g from-node)]
                    (println cost)
                    (recur
                        (remove-key-and-door in g path)
                        (get path :end)
                        (+ keys 1)
                        (+ cost (get path :cost)))
                )))))
