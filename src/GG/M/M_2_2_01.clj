(ns GG.M.M-2-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(def ^:const wname "M_2_2_01")

(def ^:const margin 25)
(def ^:const point-count 600)

(defn draw
  ""
  [canvas window ^long frame _]
  (let [{:keys [phi freq-x freq-y draw-animation?]} (get-state window)
        factor-x (- (/ (width canvas) (if draw-animation? 4 2)) margin)
        factor-y (- (/ (height canvas) (if draw-animation? 4 2)) margin)
        liss(for [i (range (inc point-count))
                  :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                        x (* factor-x (m/sin (+ (* angle freq-x) (m/radians phi))))
                        y (* factor-y (m/sin (* angle freq-y)))]]
              (v/vec2 x y))]
    
    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-stroke 1.0)
        (translate (if draw-animation? (* (width canvas) 0.75) (/ (width canvas) 2))
                   (if draw-animation? (* (height canvas) 0.75) (/ (height canvas) 2)))
        (path liss))

    (when draw-animation?
      (let [x-osc (for [i (range (inc point-count))
                        :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                              x (* (- (/ (width canvas) 4) margin)
                                   (m/sin (+ (* angle freq-x) (m/radians phi))))
                              y (+ (- (/ (* -2.0 (height canvas)) 3.0) margin)
                                   (* (/ (double i) point-count)
                                      (- (/ (height canvas) 2) margin margin)))]]
                    (v/vec2 x y))
            y-osc (for [i (range (inc point-count))
                        :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
                              y (* (- (/ (height canvas) 4) margin)
                                   (m/sin (* angle freq-y)))
                              x (+ (- (/ (* -2.0 (width canvas)) 3.0) margin)
                                   (* (/ (double i) point-count)
                                      (- (/ (width canvas) 2) margin margin)))]]
                    (v/vec2 x y))

            angle (m/norm frame 0 point-count 0 m/TWO_PI)
            x (* (- (/ (width canvas) 4) margin) (m/sin (+ (* angle freq-x) (m/radians phi))))
            y (* (- (/ (height canvas) 4) margin) (m/sin (* angle freq-y)))
            fac (m/frac (/ angle m/TWO_PI))
            osc-y-x (+ (- (/ (* -2.0 (width canvas)) 3.0) margin)
                       (* fac (- (/ (width canvas) 2) margin margin)))
            osc-x-y (+ (- (/ (* -2.0 (height canvas)) 3.0) margin)
                       (* fac (- (/ (height canvas) 2) margin margin)))]

        (-> canvas
            (path x-osc)
            (path y-osc)

            (set-color :black 80)
            (line x osc-x-y x y)
            (line osc-y-x y x y)

            (set-stroke 2.0)
            (filled-with-stroke :black :white ellipse x osc-x-y 8 8)
            (filled-with-stroke :black :white ellipse osc-y-x y 8 8)
            (filled-with-stroke :black :white ellipse x y 10 10))))))


(def window (show-window {:canvas (canvas 600 600)
                          :window-name wname
                          :draw-fn #(draw %1 %2 %3 %4)
                          :state {:phi 90.0
                                  :freq-x 1.0
                                  :freq-y 2.0
                                  :draw-animation? true}}))

(defmethod key-pressed [wname \a] [_ s] (update s :draw-animation? not))

(defmethod key-pressed [wname \1] [_ s] (update s :freq-x #(max 1 (dec %))))
(defmethod key-pressed [wname \2] [_ s] (update s :freq-x inc))

(defmethod key-pressed [wname \3] [_ s] (update s :freq-y #(max 1 (dec %))))
(defmethod key-pressed [wname \4] [_ s] (update s :freq-y inc))

(defmethod key-pressed [wname virtual-key] [e s]
  (case (key-code e)
    :left (update s :phi #(+ % 15.0))
    :right (update s :phi #(- % 15.0))
    s))
