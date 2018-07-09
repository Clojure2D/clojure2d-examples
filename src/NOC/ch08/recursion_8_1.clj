(ns examples.NOC.ch08.recursion-8-1
  (:require [clojure2d.core :refer :all]))

(def cnvs (canvas 640 360))
(def window (show-window cnvs "Recursion 8_1"))

(defn draw-circle
  "Draw recursively smaller circles."
  [canvas x y r]
  (ellipse canvas x y r r true)
  (when (> r 2.0)
    (recur canvas x y (* 0.75 r))))

(with-canvas-> cnvs
  (set-background :white)
  (set-color :black)
  (draw-circle (/ (width cnvs) 2)
               (/ (height cnvs) 2)
               (width cnvs)))
