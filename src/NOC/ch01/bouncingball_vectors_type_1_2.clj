(ns examples.NOC.ch01.bouncingball-vectors-type-1.2
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol BallProto 
  (update-ball [b canvas])
  (display-ball [b canvas]))

(deftype Ball [^Vec2 position velocity]
  BallProto
  (update-ball [_ canvas]
    (let [^Vec2 nposition (v/add position velocity)
          nvelocity (v/emult velocity (Vec2. (if (< -1.0 (.x nposition) (width canvas)) 1.0 -1.0)
                                             (if (< -1.0 (.y nposition) (height canvas)) 1.0 -1.0)))]
      (Ball. nposition nvelocity)))
  (display-ball [b canvas]
    (filled-with-stroke canvas (c/gray 175) :black ellipse (.x position) (.y position) 16 16)
    b))

(defn draw
  "update and draw ball"
  [canvas _ _ state]
  (let [b (or state (Ball. (Vec2. 100.0 100.0)
                           (Vec2. 2.5 5.0)))]
    
    (set-background canvas 255 255 255)
    (-> b
        (update-ball canvas)
        (display-ball canvas))))

(def window (show-window {:canvas (canvas 200 200)
                          :window-name "Example 1-2: Bouncing Ball, with Vec2 and type!"
                          :draw-fn draw}))
