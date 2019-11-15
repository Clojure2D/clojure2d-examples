(ns GG.M.M-1-3-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(def ^:const wname "M_1_3_01")

(defn draw
  "Draw lines and dots."
  [canvas window _ _]
  (let [xs (range 0 (width canvas) 10)
        noise (r/fbm-noise {:seed (get-state window)})
        noise-x-range (/ (mouse-x window) 20)
        norm (m/make-norm 0 (width window) 0 noise-x-range)]
    
    (let [p (for [x xs]
              (v/vec2 x (* (height window) ^double (noise (norm x)))))]
      (-> canvas
          (set-background :white)
          (set-color 0 130 164)
          (set-stroke 1 :round :round)
          (path p)))
    
    (-> canvas
        (set-color :black)
        (set-stroke 1))
    
    (doseq [x xs]
      (ellipse canvas x (* (height window) ^double (noise (norm x))) 3 3))))

(def cnvs (canvas 1024 256))
(def window (show-window {:canvas cnvs
                          :window-name wname
                          :draw-fn draw
                          :state 42}))

(defmethod mouse-event [wname :mouse-released] [_ s]
  (r/irand 100000))
