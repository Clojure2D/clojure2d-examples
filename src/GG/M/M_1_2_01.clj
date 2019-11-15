(ns GG.M.M-1-2-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(def ^:const wname "M_1_2_01")
(def ^:const cnt 150)
(def ^:const ^double angle (m/radians (/ 360.0 cnt)))

(defn draw
  "Draw dots"
  [canvas window _ last-fader-x]

  (let [rng (r/rng :isaac (get-state window))
        fader-x (if (neg? (mouse-x window))
                  last-fader-x
                  (/ (mouse-x window) (double (width canvas))))]
    
    (set-background canvas :white)
    (set-color canvas 0 130 164)
    (doseq [^long i (range cnt)]
      (let [random-x (r/drandom rng (width canvas))
            random-y (r/drandom rng (height canvas))
            circle-x (+ (* 0.5 (width canvas)) (* 300.0 (m/cos (* i angle))))
            circle-y (+ (* 0.5 (height canvas)) (* 300.0 (m/sin (* i angle))))
            x (m/lerp random-x circle-x fader-x)
            y (m/lerp random-y circle-y fader-x)]
        (ellipse canvas x y 11 11)))
    fader-x))

(def window (show-window {:canvas (canvas 800 800)
                          :window-name wname
                          :state 0
                          :draw-state 300
                          :draw-fn draw}))

(defmethod mouse-event [wname :mouse-pressed] [_ s]
  (r/irand 100000))
