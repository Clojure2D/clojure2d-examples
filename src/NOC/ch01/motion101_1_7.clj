(ns examples.NOC.ch01.motion101-1-7
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol MoverProto
  (update-mover [t w h])
  (display-mover [t canvas]))

(deftype Mover [^Vec2 position
                velocity]
  MoverProto
  (update-mover [_ w h]
    (let [^Vec2 nposition (v/add position velocity)]
      (Mover. (Vec2. (m/wrap 0 w (.x nposition))
                     (m/wrap 0 h (.y nposition)))
              velocity)))
  (display-mover [m canvas]
    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (filled-with-stroke (c/gray 127) :black ellipse (.x position) (.y position) 48 48))
    m))

(defn make-mover
  "Create Mover"
  [w h]
  (Mover. (Vec2. (r/drand w) (r/drand h))
          (Vec2. (r/drand -2.0 2.0) (r/drand -2.0 2.0))))

;; run few times
(show-window (canvas 640 360) "Motion 101 (1.7)"
             (fn [canvas window _ mover]
               (let [m (or mover (make-mover (width window) (height window)))]
                 (display-mover (update-mover m (width window) (height window)) canvas))))
