(ns examples.NOC.introduction.randomwalknoise
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn calc-noise
  "Return position based on noise values."
  [tx ty w h]
  [(m/norm (r/noise tx) 0 1 0 w)
   (m/norm (r/noise ty) 0 1 0 h)])

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double tx ^double ty ^double x ^double y] (or state (concat [0 10000] (calc-noise 0 10000 (width canvas) (height canvas))))
        [^double nx ^double ny] (calc-noise tx ty (width canvas) (height canvas))]

    (-> canvas
        (set-color :white)
        (line x y nx ny))

    [(+ tx 0.01) (+ ty 0.01) nx ny]))

(show-window (black-canvas 640 360) "Random Walk - Levy" draw)
