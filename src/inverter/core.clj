(ns inverter.core
  (:require [clojure.tools.trace])
  (:gen-class))

;; TODO assert thresholds starts 0
(defn fwd
  [{:keys [x-intercept y-intercept thresholds slopes] :as d} x]
  {:pre [(= (count slopes) (count thresholds))]}
  (cond
    (not (zero? x-intercept)) (fwd (-> d (assoc :x-intercept 0)) (- x x-intercept))
    (not (zero? y-intercept)) (+ y-intercept (fwd (-> d (assoc :y-intercept 0)) x))
    (zero? x)       0
    (= 1 (count thresholds)) (+ y-intercept (* (first slopes) x))
    (< 1 (count thresholds)) (let [x-used (min x (- (second thresholds) (first thresholds)))
                          new-d  (-> d
                                    (update :slopes rest)
                                    (update :thresholds rest)
                                    (assoc :y-intercept 0))]
                      (+ y-intercept (* (first slopes) x-used) (fwd new-d (- x x-used)))))
)

(defn invert
  [d]
  {:x-intercept (:y-intercept d)
   :y-intercept (:x-intercept d)
   :slopes      (map #(/ 1 %) (:slopes d))
   :thresholds  ( map #(- (fwd d (- % (:x-intercept d))) (:y-intercept d)) (:thresholds d))}
  )

(defn back [d x]
  (fwd (invert d) x)
  )

(defn -main
  [& args]
  (println "Hello, World!"))

(clojure.tools.trace/trace-ns 'inverter.core)
