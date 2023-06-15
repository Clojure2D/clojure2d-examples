(ns GG.M.M-2-2-01
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(def ^:const wname "M_2_2_01")

(def ^:const margin 25)
(def ^:const point-count 600)

(defn draw
  [canvas window ^long frame _]
  (let [{:keys [phi freq-x freq-y draw-animation?]} (c2d/get-state window)
        factor-x (- (/ (c2d/width canvas) (if draw-animation? 4 2)) margin)
        factor-y (- (/ (c2d/height canvas) (if draw-animation? 4 2)) margin)
        liss (for [i (range (inc point-count))
                   :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                         x (* factor-x (m/sin (+ (* angle freq-x) (m/radians phi))))
                         y (* factor-y (m/sin (* angle freq-y)))]]
               (v/vec2 x y))]
    
    (-> canvas
        (c2d/set-background :white)
        (c2d/set-color :black)
        (c2d/set-stroke 2.0)
        (c2d/translate (if draw-animation? (* (c2d/width canvas) 0.75) (/  (c2d/width canvas) 2))
                       (if draw-animation? (* (c2d/height canvas) 0.75) (/ (c2d/height canvas) 2)))
        (c2d/path liss))

    (when draw-animation?
      (let [x-osc (for [i (range (inc point-count))
                        :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                              x (* (- (/ (c2d/width canvas) 4) margin)
                                   (m/sin (+ (* angle freq-x) (m/radians phi))))
                              y (+ (- (/ (* -2.0 (c2d/height canvas)) 3.0) margin)
                                   (* (/ (double i) point-count)
                                      (- (/ (c2d/height canvas) 2) margin margin)))]]
                    (v/vec2 x y))
            y-osc (for [i (range (inc point-count))
                        :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                              y (* (- (/ (c2d/height canvas) 4) margin)
                                   (m/sin (* angle freq-y)))
                              x (+ (- (/ (* -2.0 (c2d/width canvas)) 3.0) margin)
                                   (* (/ (double i) point-count)
                                      (- (/ (c2d/width canvas) 2) margin margin)))]]
                    (v/vec2 x y))

            angle (m/norm frame 0 point-count 0 m/TWO_PI)
            x (* (- (/ (c2d/width canvas) 4) margin) (m/sin (+ (* angle freq-x) (m/radians phi))))
            y (* (- (/ (c2d/height canvas) 4) margin) (m/sin (* angle freq-y)))
            fac (m/frac (/ angle m/TWO_PI))
            osc-y-x (+ (- (/ (* -2.0 (c2d/width canvas)) 3.0) margin)
                       (* fac (- (/ (c2d/width canvas) 2) margin margin)))
            osc-x-y (+ (- (/ (* -2.0 (c2d/height canvas)) 3.0) margin)
                       (* fac (- (/ (c2d/height canvas) 2) margin margin)))]

        (-> canvas
            (c2d/path x-osc)
            (c2d/path y-osc)

            (c2d/set-color :black 80)
            (c2d/line x osc-x-y x y)
            (c2d/line osc-y-x y x y)

            (c2d/set-stroke 2.0)
            (c2d/filled-with-stroke :black :white c2d/ellipse x osc-x-y 8 8)
            (c2d/filled-with-stroke :black :white c2d/ellipse osc-y-x y 8 8)
            (c2d/filled-with-stroke :black :white c2d/ellipse x y 10 10))))))


(def window (c2d/show-window {:canvas (c2d/canvas 600 600 :highest)
                            :window-name wname
                            :draw-fn #(draw %1 %2 %3 %4)
                            :state {:phi 90.0
                                    :freq-x 1.0
                                    :freq-y 2.0
                                    :draw-animation? true}}))

(defmethod c2d/key-pressed [wname \a] [_ s] (update s :draw-animation? not))

(defmethod c2d/key-pressed [wname \1] [_ s] (update s :freq-x #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \2] [_ s] (update s :freq-x inc))

(defmethod c2d/key-pressed [wname \3] [_ s] (update s :freq-y #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \4] [_ s] (update s :freq-y inc))

(defmethod c2d/key-pressed [wname c2d/virtual-key] [e s]
  (case (c2d/key-code e)
    :left (update s :phi #(+ % 15.0))
    :right (update s :phi #(- % 15.0))
    s))
