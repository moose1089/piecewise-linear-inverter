(ns inverter.core-test
  (:require [clojure.test :refer :all]
            [inverter.core :refer :all]))

(deftest a-test
  (testing "single segment"
    (let [d {:x-intercept 0 :y-intercept 2 :thresholds [0] :slopes [1]}]
      (doseq [i (range 0 100 2)]
        (is (= (back d (fwd d i)) i)))
      ))

  (testing "multi segment, no intercept, constant slopes"
    (let [d {:x-intercept 0 :y-intercept 0 :thresholds [0 2 5 8] :slopes [1 1 1 1]} ]
      (doseq [i (range 0 100 1)]
          (is         (== (back d (fwd d i)) i)))
      ))

  (testing "multi segment, no intercept, changing slopes"
    (let [d {:x-intercept 0 :y-intercept 0 :thresholds [0 2 5 8] :slopes [1 2 3 0.25]} ]
      (doseq [i (range 0 100 1)]
          (is         (== (back d (fwd d i)) i)))
      ))

  (testing "multi segment, y intercept, changing slopes"
    (let [d {:x-intercept 0 :y-intercept 10 :thresholds [0 2 5 8] :slopes [1 2 3 0.25]} ]
      (doseq [i (range 0 100 22)]
        (is         (== (back d (fwd d i)) i)))
      ))

  (testing "single segment, x intercept"
    (let [d {:x-intercept 10 :y-intercept 0 :thresholds [0] :slopes [1]} ]
      (is         (== (back d (fwd d 19)) 19))
      (is         (== (back d (fwd d 66)) 66))
      (doseq [i (range 10 100 22)]
        (is         (== (back d (fwd d i)) i)))
      ))

  (testing "multi segment, x intercept, constant slopes"
    (let [d {:x-intercept 10 :y-intercept 0 :thresholds [0 2 5 8] :slopes [1 1 1 1]} ]
      (is         (== (back d (fwd d 19)) 19))
      (is         (== (back d (fwd d 66)) 66))
      (doseq [i (range 10 100 22)]
        (is         (== (back d (fwd d i)) i)))
      ))

  (testing "multi segment, x intercept, changing slopes"
    (let [d {:x-intercept 5 :y-intercept 0 :thresholds [0 2 5 8] :slopes [1 2 3 0.25]} ]
      (is         (== (back d (fwd d 9)) 9))
      (is         (== (back d (fwd d 20)) 20))
      (doseq [i (range 10 100 22)]
        (is         (== (back d (fwd d i)) i)))
      ))


  (testing "double segment"
    (let [d {:x-intercept 0 :y-intercept 2 :thresholds [0 3] :slopes [2 (/ 1 3)]} ]
      (is (= (fwd d 3) 8))
      (is (= (fwd d 9) 10))
      (is         (= (back d (fwd d 9)) 9))
      (doseq [i (range 0 100 22)]
        (is         (= (back d (fwd d i)) i)))
      ))

  (testing "double segment x int"
    (let [d {:x-intercept 2 :y-intercept 0 :thresholds [0 3] :slopes [2 (/ 1 3)]} ]
      (is (= (fwd d 3) 2))
      (is (= (fwd d 8) 7))
      (is         (= (back d (fwd d 9)) 9))
      (doseq [i (range 3 100 22)]
        (is         (= (back d (fwd d i)) i)))
      ))

  (testing "double segment other direction "
    (let [d (invert {:x-intercept 0 :y-intercept 2 :thresholds [0 3] :slopes [2 (/ 1 3)]})]
      (is (= (back d (fwd d 2)) 2) )
      (is (= (fwd d 8) 3))
      (is (= (fwd d 10) 9))
      (is (= (back d (fwd d 9)) 9))
      (doseq [i (range 2 100 22)]
        (is         (= (back d (fwd d i)) i)))
      )))
