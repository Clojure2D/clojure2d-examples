;; Original: http://quil.info/sketches/show/example_golden-ratio-flower
;; Author: Jack Rusher

(ns quil.golden-ratio-flower
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def palette
  (cycle [(c/color 249 187  78)
          (c/color 70 162 141)
          (c/color 220 112 100)]))

(defn draw [canvas _ ^long fps _]
  (-> canvas
      (set-background 255 255 236)
      (translate (/ (width canvas) 2) (/ (height canvas) 2)))
  (doseq [^long i (range 1000)]
    (let [v (+ (mod fps 3) i)
          ang (* v m/PHI m/TWO_PI)
          r   (* (m/sqrt v) (width canvas) (/ 70))
          x   (* (m/cos ang) r)
          y   (* (m/sin ang) r)
          sz  (+ 3 (* i 0.002))]
      (set-color canvas (nth palette i))
      (ellipse canvas x y sz sz))))

(def window (show-window (canvas 500 500) "Golden ratio flower" 10 draw))
