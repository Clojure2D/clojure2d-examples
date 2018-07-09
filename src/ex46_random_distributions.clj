(ns examples.ex46-random-distributions
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(def cnvs (canvas 1200 300))
(def window (show-window {:window-name "ex46"
                          :canvas cnvs
                          :state (cycle '(:beta :binomial :cauchy :chi-squared :exponential :f :gamma :geometric :gumbel :hypergeometric :laplace :levy :log-normal :logistic :nakagami :normal :pareto :pascal :poisson :t :triangular :uniform-real :weibull :zipf))}))

(defn draw-distribution-on-canvas
  ""
  [canvas d]
  (set-color canvas :white 15)
  (dotimes [i 1000000]
    (let [y (r/drand 300)
          x (+ 600 (* 50.0 (r/drandom d)))]
      (point canvas x y)))
  canvas)

(defn draw-distribution
  ""
  [dist-name]
  (with-canvas-> cnvs
    (set-background :black)
    (set-color :red)
    (text (name dist-name) 10 15)
    (draw-distribution-on-canvas (r/distribution dist-name))
    (set-color :lightgreen)
    (text (name dist-name) 10 15))
  (println "done"))

(defmethod key-pressed ["ex46" \space] [_ s]
  (draw-distribution (first s))
  (rest s))

(draw-distribution :normal)
