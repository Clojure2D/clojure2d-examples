(ns GG.P.P-2-0-03
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw figures"
  [canvas window _ _]
  (when (and (mouse-pressed? window) (pos? (mouse-x window)))
    (let [circle-res (int (m/norm (+ 100 (mouse-y window)) 0 (height canvas) 2 10))
          radius (+ 0.5 (- (mouse-x window) (* 0.5 (width canvas))))
          angle (/ m/TWO_PI circle-res)
          p (map #(v/vec2 (* radius (m/cos (* ^long % angle)))
                          (* radius (m/sin (* ^long % angle)))) (range circle-res))]
      (-> canvas
          (set-stroke 2.0)
          (set-color (get-state window))
          (translate (* 0.5 (width window)) (* 0.5 (height window)))
          (path p true)))))

(def cnvs (canvas 720 720))
(def window
  (do
    (with-canvas-> cnvs (set-background :white))
    (show-window {:canvas cnvs
                  :window-name "P_2_0_02"
                  :draw-fn draw
                  :fps 30
                  :state (c/color 0 0 0 25)})))

(defmethod key-pressed [(:window-name window) \backspace] [_ state]
  (with-canvas-> cnvs (set-background :white))
  state)

(defmethod key-pressed [(:window-name window) \1] [_ _] (c/color 0 0 0 25))
(defmethod key-pressed [(:window-name window) \2] [_ _] (c/from-HSB* (c/color 136 255 164 25)))
(defmethod key-pressed [(:window-name window) \3] [_ _] (c/from-HSB* (c/color 37 255 182 25)))
