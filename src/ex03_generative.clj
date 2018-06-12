(ns ex03-generative
  "Draw lines based on noise"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def foreground (c/awt-color 240 240 240 100)) ;; color for object
(def background (c/awt-color 10 20 40 100)) ;; color for background

(defn draw-lines
  "Draw shape."
  [canvas ^long framecount]
  (translate canvas 300 0) ;; go to the mid width
  (rotate canvas (* 0.25 (m/qsin (/ framecount 50.0)))) ;; sway
  (let [off (/ framecount 150.0)
        yratio (m/norm (m/qsin (* 0.1 off m/TWO_PI)) -1.0 1.0 20.0 500.0)] ;; get noise offset based on frame count
    (loop [y (double 100.1)] ;; draw several lines
      (let [n (m/norm (r/vnoise (+ (/ 1.0 (- 520.0 yratio)) (/ y yratio)) off) 0.0 1.0 -300.0 300.0)] ;; take value noise and normalize it
        (line canvas 0.0 y n y)) ;; draw line
      (when (< y 500) (recur (+ y 5.0)))))
  canvas)

(defn draw
  "Draw frame"
  [canvas window framecount _]
  (set-awt-color canvas foreground) ;; set foreground color
  (set-stroke canvas 1.85) ;; set line width
  (set-awt-background canvas background) ;; clear window
  (draw-lines canvas framecount)) ;; draw shape

(def window (show-window
             (canvas 600 600 :highest) ;; best quality canvas
             "clojure-canvas" ;; window name
             #(draw %1 %2 %3 %4))) ;; draw callback, wrap into fn to enable live coding (change code on the fly!)

(save (resize (get-image window) 300 300)
      "results/ex03/lines.jpg")

;; [[../results/ex03/lines.jpg]]
