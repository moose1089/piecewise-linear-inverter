(ns inverter.core-test
  (:require [clojure.test :refer :all]
            [inverter.core :refer :all]))

(deftest a-test
  (testing "single segment"
    (let [d {:start-y 0 :thresholds [0] :slopes [1]}]
      (doseq [i (range 0 100 3)]
        (is (= (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest b-test
  (testing "multi segment, no intercept, constant slopes"
    (let [d {:start-y 0 :thresholds [0 2 5 8] :slopes [1 1 1 1]} ]
      (doseq [i (range 0 100 1)]
          (is (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest c-test
  (testing "multi segment, no intercept, changing slopes"
    (let [d {:start-y 0 :thresholds [0 2 5 8] :slopes [1 2 3 0.25]}]
      (doseq [i (range 0 100 1)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i))))))

(deftest d-test
  (testing "multi segment, y intercept, changing slopes"
    (let [d {:start-y 10 :thresholds [0 2 5 8] :slopes [1 2 3 0.25]} ]
      (doseq [i (range 0 100 22)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest e-test
  (testing "multi segment, x and y intercept, changing slopes"
    (let [d {:start-y 10 :thresholds [10 12 15 18] :slopes [1 2 3 0.25]} ]
      (doseq [i (range 12 100 22)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest f-test
  (testing "single segment, x intercept"
    (let [d {:start-y 0 :thresholds [10] :slopes [1]} ]
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 19)) 19))
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 66)) 66))
      (doseq [i (range 10 100 22)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest g-test
  (testing "multi segment, x intercept, constant slopes"
    (let [d {:start-y 0 :thresholds [10 12 15 18] :slopes [1 1 1 1]} ]
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 19)) 19))
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 66)) 66))
      (doseq [i (range 10 100 22)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest h-test
  (testing "multi segment, x intercept, changing slopes"
    (let [d {:start-y 0 :thresholds [10 12 15 18] :slopes [1 2 3 0.25]} ]
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 12)) 12))
      (is         (== (eval-inverse-PWLF d (eval-PWLF d 20)) 20))
      (doseq [i (range 10 100 22)]
        (is         (== (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest i-test
  (testing "double segment"
    (let [d {:start-y 2 :thresholds [0 3] :slopes [2 (/ 1 3)]} ]
      (is (= (eval-PWLF d 3) 8))
      (is (= (eval-PWLF d 9) 10))
      (is         (= (eval-inverse-PWLF d (eval-PWLF d 9)) 9))
      (doseq [i (range 0 100 22)]
        (is         (= (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest j-test
  (testing "double segment x int"
    (let [d {:start-y 0 :thresholds [2 5] :slopes [2 (/ 1 3)]} ]
      (is (= (eval-PWLF d 3) 2))
      (is (= (eval-PWLF d 8) 7))
      (is         (= (eval-inverse-PWLF d (eval-PWLF d 9)) 9))
      (doseq [i (range 3 100 22)]
        (is         (= (eval-inverse-PWLF d (eval-PWLF d i)) i)))
      )))

(deftest k-test
  (testing "double segment other direction "
    (let [d (invert-PWLF {:start-y 2 :thresholds [0 3] :slopes [2 (/ 1 3)]})]
      (is (= (eval-inverse-PWLF d (eval-PWLF d 2)) 2))
      (is (= (eval-PWLF d 8) 3))
      (is (= (eval-PWLF d 10) 9))
      (is (= (eval-inverse-PWLF d (eval-PWLF d 9)) 9))
      (doseq [i (range 2 100 22)]
        (is         (= (eval-inverse-PWLF d (eval-PWLF d i)) i))))))
