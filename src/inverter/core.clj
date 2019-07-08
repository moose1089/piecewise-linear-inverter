(ns inverter.core
  (:require [clojure.tools.trace])
  (:gen-class))

;; NOTE PWLF = Piece-wise linear function, it starts from (first thresholds, start-y), is defined on ((first thresholds), infinity)

(defn eval-PWLF
  "This evaluates the piecewise linear function for the provided x.
  The definition of the function in d containing a map of
  :slopes - the vector giving the slopes for each segment. A 0 slope cannot be inverted.
  :thresholds - the x at which each segment starts.
  providing x < first threshold is an error.

  The evaluation is largely recursive, progressively simplifying the complex function into a simple linear function by the following steps:
  1 - remove y intercept
  2 - translate x so thresholds to start at 0
  3 - remove a single segment by using up the part of x between the first 2 thresholds.
  "
  [{:keys [start-y thresholds slopes] :as d} x]
  {:pre [(= (count slopes) (count thresholds))]}
  (cond
    (not (zero? start-y))            (+ start-y (eval-PWLF (-> d (assoc :start-y 0)) x))
    (not (zero? (first thresholds))) (eval-PWLF (-> d (assoc :thresholds (map #(- % (first thresholds)) thresholds))) (- x (first thresholds)))
    (< x 0)                          (throw (ex-info (str "out out scope" {:x x :d d})  {:x x :d d}))
    (< 1 (count thresholds))         (let [x-used (min x (- (second thresholds) (first thresholds)))
                                           new-d  (-> d
                                                     (update :slopes rest)
                                                     (update :thresholds (fn [t] (map #(- % (second thresholds)) (rest t))))
                                                     (assoc :start-y 0))]
                                       (+ (* (first slopes) x-used) (eval-PWLF new-d (- x x-used))))
    :simple-case                     (* (first slopes) x)))

(defn invert-PWLF
  "Invert the definition of the piecewise linear function provided by d
  Slopes are simply inverted, i.e. 1/slope. 0 slopes cannot be inverted as there is no unique inverse.
  Thresholds come from the evaluation of the function, i.e. the input definition y's become the threshold x's of the output definition.
  "
  [{:keys [start-y thresholds slopes] :as d}]
  {:start-y    (first thresholds)
   :slopes     (map #(/ 1 %) (:slopes d))
   :thresholds (->> d
                  :thresholds
                  (map (partial eval-PWLF d))
                  )})

(defn eval-inverse-PWLF [d x]
  (eval-PWLF (invert-PWLF d) x)
  )

(defn -main
  [& args]
  (println "Hello, World!"))

;(clojure.tools.trace/trace-ns 'inverter.core)
