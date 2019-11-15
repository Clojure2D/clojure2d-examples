(ns GG.P.P-2-0-02
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

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
          (set-color :black 25)
          (translate (* 0.5 (width window)) (* 0.5 (height window)))
          (path p true)))))

(def cnvs (canvas 720 720))
(def window
  (do
    (with-canvas-> cnvs (set-background :white))
    (show-window {:canvas cnvs
                  :window-name "P_2_0_02"
                  :draw-fn draw
                  :state false})))

(defmethod key-pressed [(:window-name window) \backspace] [_ state]
  (with-canvas-> cnvs (set-background :white))
  state)
