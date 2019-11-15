(ns examples.NOC.ch01.vector-subtraction-1-3
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(show-window
 (canvas 640 360)
 "Example 1-3: Vector subtraction"
 (fn [canvas window _ _]
   (let [center (Vec2. (/ (width window) 2) (/ (height window) 2))
         ^Vec2 mouse (v/sub (mouse-pos window) center)]

     (-> canvas
         (set-background :white)
         (translate (.x center) (.y center))
         (set-stroke 2.0)
         (set-color :black)
         (line 0 0 (.x mouse) (.y mouse))))))

;; Note: when you go outside window (mouse-pos window) returns [-1,-1].
