(ns examples.NOC.introduction.noisewalk-I-6
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def step (v/vec2 0.005 0.005))

(defn draw
  ""
  [canvas _ _ state]
  (let [^Vec2 noff (or state (v/vec2 (r/drand 1000) (r/drand 1000)))

        x (m/norm (r/noise (.x noff)) 0.0 1.0 0.0 (width canvas))
        y (m/norm (r/noise (.y noff)) 0.0 1.0 0.0 (height canvas))]

    (-> canvas
        (set-background :white)
        (set-color 127 127 127)
        (ellipse x y 48 48)
        (set-color :black)
        (ellipse x y 48 48 true))

    (v/add noff step)))

(show-window (canvas 800 200) "Noise Walk I_6" 30 draw)
