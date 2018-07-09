(ns examples.NOC.ch01.bouncingball-novectors-1-1
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn boundary-check
  "Returns -1.0 if out of borders, 1.0 otherwise"
  ^double [^double mx ^double v]
  (if (< -1.0 v mx) 1.0 -1.0))

(defn draw
  "Bounce ball"
  [canvas _ _ state]
  (let [[^double x ^double y ^double xspeed ^double yspeed] (or state [100 100 2.5 2.0])
        nx (+ x xspeed)
        ny (+ y yspeed)]

    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (filled-with-stroke (c/gray 127) :black ellipse nx ny 48 48))
    
    [nx ny
     (* xspeed (boundary-check (width canvas) nx))
     (* yspeed (boundary-check (height canvas) ny))]))

(def window (show-window (canvas 800 200) "Example 1-1: Bouncing Ball, no vectors" draw))
