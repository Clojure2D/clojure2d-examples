(ns NOC.ch03.static-wave-lines-3-8
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defn create-shape
  "Create sin shape"
  []
  (map-indexed #(vector %2 (m/norm (m/sin (* 0.1 ^double %1)) -1.0 1.0 0 h)) (range 0 (inc w) 5)))

(def cnvs (canvas w h))
(def window (show-window cnvs "Static wave lines 3_8"))

(with-canvas-> cnvs
  (set-background :white)
  (set-color :black)
  (set-stroke 2.0)
  (path (create-shape)))
