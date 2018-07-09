(ns examples.NOC.ch08.recursion-8-2
  (:require [clojure2d.core :refer :all]))

(def cnvs (canvas 640 360))
(def window (show-window cnvs "Recursion 8_2"))

(defn draw-circle
  ""
  [canvas x y r]
  (ellipse canvas x y r r true)
  (when (> r 2.0)
    (let [r2 (/ r 2)]
      (draw-circle canvas (+ x r2) y r2)
      (draw-circle canvas (- x r2) y r2))))

(with-canvas-> cnvs
  (set-background :white)
  (set-color :black)
  (draw-circle (/ (width cnvs) 2)
               (/ (height cnvs) 2)
               400))
