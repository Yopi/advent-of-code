(ns adventofcode.2018.day13
    (:require
        [clojure.java.io :as io]
        [clojure.string :as str]))

(defn convert-cart-to-rail [o]
    (case o
        "<" "-"
        ">" "-"
        "^" "|"
        "v" "|"
        o))

(defn col-to-rail [x o]
    [(list x) (convert-cart-to-rail o)])

(defn row-to-rails [y row]
    (map (fn [obj] (assoc obj 0 (conj (first obj) y)))
        (map-indexed col-to-rail (str/split row #""))))

(defn col-to-cart [x o]
    (case o
        ("<" ">" "^" "v") {:x x :cart o :turns 0 :id x}
        nil))

(defn row-to-carts [y row]
    (map (fn [obj] (assoc obj :y y :id (str (get obj :x) y)))
        (map-indexed col-to-cart (str/split row #""))))


(def testrails
    (reduce into {}
        (map-indexed row-to-rails
            (str/split (-> "day13/example-internet.txt"
                            io/resource
                            io/file
                            slurp)
            #"\n"))))

(def testcarts
    (vec (filter #(> (count %) 2) (reduce into []
            (map-indexed row-to-carts
                (str/split (-> "day13/example-internet.txt"
                                io/resource
                                io/file
                                slurp)
                #"\n"))))))

(def realrails
    (reduce into {}
        (map-indexed row-to-rails
            (str/split (-> "day13/input.txt"
                            io/resource
                            io/file
                            slurp)
            #"\n"))))

(def realcarts
    (vec (filter #(> (count %) 2) (reduce into []
            (map-indexed row-to-carts
                (str/split (-> "day13/input.txt"
                                io/resource
                                io/file
                                slurp)
                #"\n"))))))

(def rails realrails)
(def carts realcarts)

; Helper functions
(defn sort-input [input]
    (vec (flatten (sort-by (juxt :y :x) input))))

(defn cart-at [carts x y]
    (first (filter #(and (= (get % :x) x) (= (get % :y) y)) carts)))

(defn all-carts-at [carts x y]
    (filter #(and (= (get % :x) x) (= (get % :y) y)) carts))

(defn index-of-cart [carts cart]
  (loop [carts carts
         i 0]
    (cond
      (= cart (first carts)) i
      (empty? carts)        nil
      :else                 (recur (rest carts) (inc i)))))

(defn get-collided-carts [carts]
    (->> carts
        (group-by #(select-keys % [:x :y]))
        (remove #(= 1 (count (val %))))
        (map second)
        (flatten)))

(defn drop-collided-from [carts x y]
    (vec (remove #(and (= (get % :x) x) (= (get % :y) y)) carts)))

(defn drop-all-collided-from [carts collided]
    (loop [remaining-carts carts
            remaining-collided collided]
            (if (empty? remaining-collided)
                remaining-carts
                (let [coords (select-keys (first remaining-collided) [:x, :y])]
                    (recur (drop-collided-from remaining-carts (get coords :x) (get coords :y)) (rest remaining-collided))))))

(defn rail-at [rails x y]
    (let [r (get rails (list y x))]
        (if (nil? r)
            " "
            r)))

(defn inc-with-max [nbr mx]
    (if (> nbr mx)
        0
        (inc nbr)))

(def height (apply max (map first (keys rails))))
(def width (apply max (map second (keys rails))))

(defn slash-turn [cart]
    ;; /
    (case (get cart :cart)
        "<" (assoc cart :cart "v")
        ">" (assoc cart :cart "^")
        "^" (assoc cart :cart ">")
        "v" (assoc cart :cart "<")))


(defn backslash-turn [cart]
    ;; \
    (case (get cart :cart)
        "<" (assoc cart :cart "^")
        ">" (assoc cart :cart "v")
        "^" (assoc cart :cart "<")
        "v" (assoc cart :cart ">")))

(defn turn-left [direction]
    (case direction
        "<" "v"
        ">" "^"
        "^" "<"
        "v" ">"))

(defn turn-right [direction]
    (case direction
        "<" "^"
        ">" "v"
        "^" ">"
        "v" "<"))

; Each time a cart has the option to turn (by arriving at any intersection),
; it turns left the first time,
; goes straight the second time,
; turns right the third time,
; and then repeats those directions starting again with left the fourth time, straight the fifth time
(defn intersection-turn [cart]
    (case (get cart :turns)
        0 (assoc cart :cart (turn-left (get cart :cart)) :turns (inc-with-max (get cart :turns) 1))
        1 (assoc cart :turns (inc-with-max (get cart :turns) 1))
        2 (assoc cart :cart (turn-right (get cart :cart)) :turns (inc-with-max (get cart :turns) 1))
    cart))

(defn new-cart [[x y] cart]
    (as-> cart c
        (assoc c :x (+ (get c :x) x))
        (assoc c :y (+ (get c :y) y))
        (case (get rails (list (get c :y) (get c :x)))
            "/" (slash-turn c)
            "\\" (backslash-turn c)
            "+" (intersection-turn c)
            c)))

(defn move-cart [cart]
    (case (get cart :cart)
        "<" (new-cart [-1 0] cart)
        ">" (new-cart [1 0] cart)
        "^" (new-cart [0 -1] cart)
        "v" (new-cart [0 1] cart)
    nil))

(defn print-field [cs]
    (println
        (str/join "\n"
            (map #(str/join "" %)
             (for [y (range (inc height))]
                (for [x (range (inc width))]
                    (let [c (cart-at cs x y)
                            r (rail-at rails x y)]
                        (if-not (nil? c)
                            (get c :cart)
                            r))))))))

(defn tick [local-carts]
    (loop [cs (sort-input local-carts)
            tracking-cs cs]
        (if (and (<= (count cs) 1) (empty? tracking-cs))
            cs ;; Last cart standing
            (if (empty? tracking-cs)
                (recur (sort-input cs) (sort-input cs)) ; Next tick
                (let [updated-cs (assoc cs (index-of-cart cs (first tracking-cs)) (move-cart (first tracking-cs)))]
                    (let [collided (get-collided-carts updated-cs)]
                        (if-not (empty? collided)
                            (do
                                (println "Collision")
                                (println cs)
                                (println collided)
                                (recur (drop-all-collided-from updated-cs collided) (rest (drop-all-collided-from tracking-cs collided))))
                            (recur updated-cs (sort-input (rest tracking-cs))))))))))
