(ns matross.strata-test
  (:require [clojure.test :refer :all]
            [matross.strata :refer :all]))

(defn test-strata [& ms]
  (reduce (fn [r m] (add-stratum r
                     (str (java.util.UUID/randomUUID))
                     m)) (strata) ms))

(deftest strata-behaves-like-a-map
  (let [s (test-strata {:k :not-v} {:k :v :foo :bar} {:baz :bat})]
    (testing "I can pass it to a 'keyword function'"
      (is (= (:k s) :v)))

    (testing "I can call it as a function"
      (is (= (s :k) :v)))
    
    (testing "I can count the entries"
      (is (= (count s) 3)))

    (testing "I can conj an element onto it"
      (let [conj-s (conj s [:foo :baz])]
        (is (= (:foo conj-s) :baz))))

    (testing "I can assoc an element into it"
      (let [assoc-s (assoc s :foo :baz)]
        (is (= (:foo assoc-s) :baz))))

    (testing "I can dissoc an element"
      (let [dissoc-s (dissoc s :k)]
        (is (not (:k dissoc-s)))))

    (testing "I can call `keys` on strata"
      (is (= (count (keys s)) 3)))

    (testing "I can call `vals` on strata"
      (is (= (vals s) '(:bar :v :bat))))))
