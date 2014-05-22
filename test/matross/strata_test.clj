(ns matross.strata-test
  (:require [clojure.test :refer :all]
            [matross.strata :refer :all]))

(defn test-strata [& ms]
  [(apply conj (strata-lifo) ms)
   (apply conj (strata-fifo) ms)])

(deftest strata-behaves-like-a-map
  (let [[lifo fifo] (test-strata {:k :not-v} {:k :v :foo :bar} {:baz :bat})]
    (testing "I can pass it to a 'keyword function'"
      (is (= (:k lifo) :v))
      (is (= (:k fifo) :not-v)))

    (testing "I can call it as a function"
      (is (= (lifo :k) :v))
      (is (= (fifo :k) :not-v)))
    
    (testing "I can count the entries"
      (is (= (count lifo) 3)))

    (testing "I can conj an element onto it"
      (let [conj-lifo (conj lifo [:foo :baz])
            conj-fifo (conj fifo [:foo :baz])]
        (is (= (:foo conj-lifo) :baz))
        (is (= (:foo conj-fifo) :bar))))

    (testing "I can assoc an element into it"
      (let [assoc-lifo (assoc lifo :foo :baz)
            assoc-fifo (assoc fifo :foo :baz)]
        (is (= (:foo assoc-lifo) :baz))
        (is (= (:foo assoc-fifo) :bar))))

    (testing "I can dissoc an element"
      (let [dissoc-lifo (dissoc lifo :k)
            dissoc-fifo (dissoc fifo :k)]
        (is (not (:k dissoc-lifo)))
        (is (not (:k dissoc-fifo)))))

    (testing "I can call `keys` on strata"
      (is (= (count (keys lifo)) 3)))

    (testing "I can call `vals` on strata"
      (is (= (vals lifo) '(:bar :v :bat)))
      (is (= (vals fifo) '(:bar :not-v :bat))))))
