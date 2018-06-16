(ns example.NOC.introduction.figure-I-6-randomgraph
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def cnvs (canvas 400 200))
(def window (show-window cnvs "Random graph"))

(let [p (map #(v/vec2 % (r/drand (height cnvs))) (range 0 (width cnvs) 2))]
  (with-canvas-> cnvs
    (set-background :white)
    (set-color :black)
    (set-stroke 2.0)
    (path p)))
