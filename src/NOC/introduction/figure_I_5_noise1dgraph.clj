(ns example.NOC.introduction.figure-I-5-noise1dgraph
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw on canvas."
  [canvas window framecount state]
  (let [^double t (or state 0.0)
        p (map #(v/vec2 % (* (height window) (r/noise (+ t (* 0.01 ^double %))))) (range 0 (width canvas) 2))]

    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-stroke 2.0)
        (path p))

    (+ t 0.01)))

(def window (show-window (canvas 400 200) "Noise 1d graph" draw))
