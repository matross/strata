# Strata

Strata is a library that allows you to effectively merge multiple maps together while keeping information
about where each value came from. It is intended to help with debugging key values that come from multiple
sources, eg defaults, configuration files, and commandline options.


## Usage

```clojure
(require '[matross.strata :refer [strata stratum enable-debug])

(let [config  (-> (strata)
               (conj (stratum "first-map"  {:foo "bar" :baz "bat"}))
               (conj (stratum "second-map" {:foo "not-bar"})))]
  (enable-debug)
  (println "The value of foo is: " (:foo config)))
  ; Found key `:foo` in "first-map"
  ; => "not-bar"
```

## License

Copyright © 2014 Darrell Hamilton

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
