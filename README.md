# Advent of Code 2018

(Hopefully!) Clojure solutions to Advent of Code 2018

## Usage

- Install Clojure and leiningen: `brew install clojure && brew install leiningen`.

- Install dependencies via leiningen: `lein deps`

- Start the REPL `lein repl`

- Load the day you want, jump into the namespace and run the solution. E.g:

```clojure
(load-file "src/adventofcode/day01.clj")

(in-ns adventofcode.day01)

(println part1)
```
