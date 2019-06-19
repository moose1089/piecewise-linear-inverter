(ns inverter.core
  (:require [clojure.tools.trace])
  (:gen-class))

;; NOTE PWLF = Piece-wise linear function

;; TODO assert thresholds starts 0
(defn eval-PWLF
  [{:keys [x-intercept y-intercept thresholds slopes] :as d} x]
  {:pre [(= (count slopes) (count thresholds))]}
  (cond
    (not (and (zero? x-intercept) (zero? y-intercept))) (+ y-intercept (eval-PWLF (-> d (assoc :x-intercept 0 :y-intercept 0)) (- x x-intercept) ))
    (< x 0)                   (throw (ex-info (str "out out scope" {:x x :d d})  {:x x :d d}))
    (= 1 (count thresholds))  (+ y-intercept (* (first slopes) x))
    (< 1 (count thresholds))  (let [x-used (min x (- (second thresholds) (first thresholds)))
                                    new-d  (-> d
                                              (update :slopes rest)
                                              (update :thresholds rest)
                                              (assoc :y-intercept 0))]
                                (+ (* (first slopes) x-used) (eval-PWLF new-d (- x x-used)))))
)

(defn invert-PWLF
  [{:keys [x-intercept y-intercept thresholds slopes] :as d}]
  {:x-intercept (:y-intercept d)
   :y-intercept (:x-intercept d)
   :slopes      (map #(/ 1 %) (:slopes d))
   :thresholds  (map #(- % y-intercept ) (map (fn [x] (eval-PWLF d x)) (map #(+ x-intercept %) (:thresholds d))))}
  )

(defn eval-inverse-PWLF [d x]
  (eval-PWLF (invert-PWLF d) x)
  )

(defn -main
  [& args]
  (println "Hello, World!"))

;(clojure.tools.trace/trace-ns 'inverter.core)
