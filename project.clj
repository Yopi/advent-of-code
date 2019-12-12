(defproject adventofcode "0.1"
  :description "Advent of code 2018"
  :url "http://example.com/FIXME"
  :license {:name "The Unlicense"
            :url "https://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.6.532"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [ubergraph "0.8.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :profiles {:dev {:resource-paths ["resources"]}}
  :jvm-opts ["-Xmx8G"])
