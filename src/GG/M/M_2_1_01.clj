(ns GG.M.M-2-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(def ^:const wname "M_2_1_01")

(defn draw
  ""
  [canvas window ^long frame _]
  (let [{:keys [phi ^double freq draw-animation?]} (get-state window)
        point-count (if draw-animation? (- (width canvas) 250) (width canvas))
        shape (for [i (range point-count)
                    :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                          y (m/sin (+ (* angle freq) (m/radians phi)))]]
                (v/vec2 i (* y 100.0)))]
    
    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-stroke 2.0)
        (translate (if draw-animation? 250 0) (/ (height canvas) 2))
        (path shape))

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

            (set-stroke 1.0)
            (ellipse -125 0 200 200 true)

            (set-color :black 128)
            (line 0 -100 0 100)
            (line 0 0 point-count 0)
            (line -225 0 -25 0)
            (line -125 -100 -125 100)
            (line x y -125 0)

            (set-color 0 130 164)
            (set-stroke 2.0)
            (line tpc y tpc 0)
            (line x y x 0)

            (set-stroke 1.0)
            (set-color :black 128)
            (line -125 0 phi-x phi-y)

            (set-stroke 2.0)
            (filled-with-stroke :black :white ellipse 0 phi-y 8 8)
            (filled-with-stroke :black :white ellipse phi-x phi-y 8 8)
            (filled-with-stroke :black :white ellipse tpc y 10 10)
            (filled-with-stroke :black :white ellipse x y 10 10))))))


(def window (show-window {:canvas (canvas 800 400)
                          :window-name wname
                          :draw-fn #(draw %1 %2 %3 %4)
                          :state {:phi 0.0
                                  :freq 1.0
                                  :draw-animation? true}}))

(defmethod key-pressed [wname \a] [_ s] (update s :draw-animation? not))

(defmethod key-pressed [wname \1] [_ s] (update s :freq #(max 1 (dec ^double %))))
(defmethod key-pressed [wname \2] [_ s] (update s :freq inc))

(defmethod key-pressed [wname virtual-key] [e s]
  (case (key-code e)
    :left (update s :phi #(+ ^double % 15.0))
    :right (update s :phi #(- ^double % 15.0))
    s))
