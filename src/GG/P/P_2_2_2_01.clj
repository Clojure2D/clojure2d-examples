(ns GG.P.P-2-2-2-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]))

(def ^:const ^double angle-count 7.0)
(def ^:const ^double step-size 3.0)
(def ^:const ^double min-length 10.0)

(def wname "P_2_2_2_01")

(defn get-random-angle
  ""
  [the-direction]
  (let [a (* (/ 90.0 angle-count) (m/floor (+ 0.5 (r/drand (- angle-count) angle-count))))]
    (case the-direction
      :north (- a 90.0)
      :east a
      :south (+ a 90.0)
      :west (+ a 180.0)
      0.0)))

(defn draw-helper
  "Recurrent version of draw (to loop)"
  [canvas ^long times [^double posx ^double posy ^double posx-cross ^double posy-cross ^double angle direction :as all]]
  (if (neg? times)
    all
    (let [posx (+ posx (* step-size (m/cos (m/radians angle))))
          posy (+ posy (* step-size (m/sin (m/radians angle))))
          [direction reached-border] (cond
                                       (<= posy 5.0) [:south true]
                                       (>= posx (- (width canvas) 5.0)) [:west true]
                                       (<= posx 5.0) [:east true]
                                       (>= posy (- (height canvas) 5.0)) [:north true]
                                       :else [direction false])
          px (int posx)
          py (int posy)
          [angle posx-cross posy-cross] (if (or (< (c/luma (p/get-color canvas px py)) 255)
                                                reached-border) 
                                          (let [angle (get-random-angle direction) 
                                                distance (m/dist posx posy posx-cross posy-cross)]
                                            (when (>= distance min-length)
                                              (line canvas posx posy posx-cross posy-cross))
                                            [angle posx posy])
                                          [angle posx-cross posy-cross])]
      (recur canvas (dec times) [posx posy posx-cross posy-cross angle direction]))))


(defn draw
  ""
  [canvas window _ state]
  (if (get-state window)
    (do
      (set-color canvas :black)
      (set-stroke canvas 3.0)
      (draw-helper canvas (max 1 (mouse-x window)) state))
    state))

(def cnvs (canvas 600 600))

(with-canvas-> cnvs (set-background :white))

(def window (let [posx (r/irand (width cnvs))
                  posy 5.0]
              (show-window {:window-name wname
                            :canvas cnvs
                            :draw-fn draw
                            :state true
                            :draw-state [posx posy posx posy 
                                         (get-random-angle :south)
                                         :south]})))

(defmethod key-released [wname \backspace] [_ s]
  (with-canvas-> cnvs (set-background :white))
  s)

(defmethod key-released [wname \space] [_ _] false)
(defmethod key-released [wname \l] [_ _] true)
