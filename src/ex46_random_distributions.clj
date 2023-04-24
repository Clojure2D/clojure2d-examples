(ns ex46-random-distributions
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]))

(def cnvs (c2d/canvas 1200 300))
(def window (c2d/show-window {:window-name "ex46"
                            :canvas cnvs
                            :state (cycle '(:beta :binomial :cauchy :chi-squared :exponential :f :gamma :geometric :gumbel :hypergeometric :laplace :levy :log-normal :logistic :nakagami :normal :pareto :pascal :poisson :t :triangular :uniform-real :weibull :zipf))}))

(defn draw-distribution-on-canvas
  [canvas d]
  (c2d/set-color canvas :white 15)
  (dotimes [_ 1000000]
    (let [y (r/drand 300)
          x (+ 600 (* 50.0 (r/drandom d)))]
      (c2d/point canvas x y)))
  canvas)

(defn draw-distribution
  [dist-name]
  (c2d/with-canvas-> cnvs
    (c2d/set-background :black)
    (c2d/set-color :red)
    (c2d/text (name dist-name) 10 15)
    (draw-distribution-on-canvas (r/distribution dist-name))
    (c2d/set-color :lightgreen)
    (c2d/text (name dist-name) 10 15))
  (println "done"))

(defmethod c2d/key-pressed ["ex46" \space] [_ s]
  (draw-distribution (first s))
  (rest s))

(draw-distribution :normal)
