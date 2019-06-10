(ns inverter.core-test
  (:require [clojure.test :refer :all]
            [inverter.core :refer :all]))

(deftest a-test
  #_(testing "single segment"
    (let [d {:x 0 :y 2 :t [0] :s [1]}]
      (is (= (fwd d 3) 5))
      (is (= (fwd d 4) 6))
      (is (= (back d 5) 3))
      (is (= (back d 6) 4))
      (doseq [i (range 0 100 22)]
        (is         (= (back d (fwd d i)) i)))
      ))

  (testing "triple segment"
    (let [d {:x-intercept 40 :y-intercept 22 :thresholds [0 3 7] :slopes [2 (/ 1 3) 5]} ]
      (is         (= (back d (fwd d 9)) 9))
      (is         (= (back d (fwd d 44)) 44))
      #_(doseq [i (range 0 100 22)]
        (is         (= (back d (fwd d i)) i)))
      ))

  #_(testing "double segment"
    (let [d {:x-intercept 0 :y-intercept 2 :thresholds [0 3] :slopes [2 (/ 1 3)]} ]
      (is (= (fwd d 3) 8))
      (is (= (fwd d 9) 10))
      (is         (= (back d (fwd d 9)) 9))
      (doseq [i (range 0 100 22)]
        (is         (= (back d (fwd d i)) i)))
      ))

  #_(testing "double segment other direction "
    (let [d (invert {:x-intercept 0 :y-intercept 2 :thresholds [0 3] :slopes [2 (/ 1 3)]})]
      (is (= (fwd d 8) 3))
      (is (= (fwd d 10) 9))
      (is         (= (back d (fwd d 9)) 9))
      (doseq [i (range 0 100 22)]
        (is         (= (back d (fwd d i)) i)))
      )))
