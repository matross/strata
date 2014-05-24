# Strata

Strata is a library that allows you to effectively merge multiple maps together while keeping information
about where each value came from. It is intended to help with debugging key values that come from multiple
sources, eg defaults, configuration files, and commandline options.

There are two variations of Strata depending on your needs, `strata-fifo` in which the first occurrence of a key
takes precedence, and `strata-lifo` in which the latest occurence of a key takes precedence.


## Usage

Strata is available on Clojars. To include in your project, simply add the following to your dependencies:

```clojure
[matross/strata "0.1.0-SNAPSHOT"]
```

```clojure
(require '[matross.strata :refer [strata stratum enable-debug])

; Strata FIFO example
(let [config  (-> (strata-fifo)
               (enable-debug)
               (conj (stratum "first-map"  {:foo "bar" :baz "bat"}))
               (conj (stratum "second-map" {:foo "not-bar"})))]
  (println "The value of foo is: " (:foo config)))
  ; Found key `:foo` in "first-map"
  ; => "bar"

; Strata LIFO example
(let [config  (-> (strata-lifo)
               (enable-debug)
               (conj (stratum "first-map"  {:foo "bar" :baz "bat"}))
               (conj (stratum "second-map" {:foo "not-bar"})))]
  (println "The value of foo is: " (:foo config)))
  ; Found key `:foo` in "second-map"
  ; => "not-bar"
```

## License

Copyright © 2014 Darrell Hamilton

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
