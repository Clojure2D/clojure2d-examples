(ns GG.P.P-2-0-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [circle-res (int (m/norm (mouse-y window) 0 (height canvas) 2 80))
        radius (+ 0.5 (- (mouse-x window) (* 0.5 (width canvas))))
        angle (/ m/TWO_PI circle-res)]
    (-> canvas
        (set-stroke (/ (max 1.0 (mouse-y window)) 20.0) :square)
        (set-background :white)
        (set-color :black)
        (translate (* 0.5 (width window)) (* 0.5 (height window))))
    (dotimes [i circle-res]
      (let [x (* radius (m/cos (* i angle)))
            y (* radius (m/sin (* i angle)))]
        (line canvas 0 0 x y)))))

(def window (show-window (canvas 550 550) "P_2_0_01" draw))
