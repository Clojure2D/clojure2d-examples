(ns GG.M.M-2-1-01
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(def ^:const wname "M_2_1_01")

(defn draw
  [canvas window ^long frame _]
  (let [{:keys [phi ^double freq draw-animation?]} (c2d/get-state window)
        point-count (if draw-animation? (- (c2d/width canvas) 250) (c2d/width canvas))
        shape (for [i (range point-count)
                    :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                          y (m/sin (+ (* angle freq) (m/radians phi)))]]
                (v/vec2 i (* y 100.0)))]
    
    (-> canvas
        (c2d/set-background :white)
        (c2d/set-color :black)
        (c2d/set-stroke 2.0)
        (c2d/translate (if draw-animation? 250 0) (/ (c2d/height canvas) 2))
        (c2d/path shape))

    (when draw-animation?
      (let [t (m/frac (/ (double frame) point-count))
            angle (* t m/TWO_PI)
            v (+ (* angle freq) (m/radians phi))
            x (- (* 100.0 (m/cos v)) 125.0)
            y (* 100.0 (m/sin v))
            tpc (* t point-count)
            phi-x (- (* 100 (m/cos (m/radians phi))) 125)
            phi-y (* 100 (m/sin (m/radians phi)))]

        (-> canvas

            (c2d/set-stroke 1.0)
            (c2d/ellipse -125 0 200 200 true)

            (c2d/set-color :black 128)
            (c2d/line 0 -100 0 100)
            (c2d/line 0 0 point-count 0)
            (c2d/line -225 0 -25 0)
            (c2d/line -125 -100 -125 100)
            (c2d/line x y -125 0)

            (c2d/set-color 0 130 164)
            (c2d/set-stroke 2.0)
            (c2d/line tpc y tpc 0)
            (c2d/line x y x 0)

            (c2d/set-stroke 1.0)
            (c2d/set-color :black 128)
            (c2d/line -125 0 phi-x phi-y)

            (c2d/set-stroke 2.0)
            (c2d/filled-with-stroke :black :white c2d/ellipse 0 phi-y 8 8)
            (c2d/filled-with-stroke :black :white c2d/ellipse phi-x phi-y 8 8)
            (c2d/filled-with-stroke :black :white c2d/ellipse tpc y 10 10)
            (c2d/filled-with-stroke :black :white c2d/ellipse x y 10 10))))))


(def window (c2d/show-window {:canvas (c2d/canvas 800 400 :highest)
                            :window-name wname
                            :draw-fn #(draw %1 %2 %3 %4)
                            :state {:phi 0.0
                                    :freq 1.0
                                    :draw-animation? true}}))

(defmethod c2d/key-pressed [wname \a] [_ s] (update s :draw-animation? not))

(defmethod c2d/key-pressed [wname \1] [_ s] (update s :freq #(max 1 (dec ^double %))))
(defmethod c2d/key-pressed [wname \2] [_ s] (update s :freq inc))

(defmethod c2d/key-pressed [wname c2d/virtual-key] [e s]
  (case (c2d/key-code e)
    :left (update s :phi #(+ ^double % 15.0))
    :right (update s :phi #(- ^double % 15.0))
    s))
