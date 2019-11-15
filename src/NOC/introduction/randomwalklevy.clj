(ns examples.NOC.introduction.randomwalklevy
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn montecarlo-fn
  ""
  ^double []
  (let [r1 (r/drand)
        probability (m/pow (- 1.0 r1) 8.0)]
    (if (< (r/drand) probability)
      r1
      -1.0)))

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 (width canvas))
                                         (* 0.5 (height canvas))])
        stepsize (* 50.0 ^double (first (filter pos? (repeatedly montecarlo-fn))))
        stepsize- (- stepsize)
        stepx (r/drand stepsize- stepsize)
        stepy (r/drand stepsize- stepsize) 
        nx (m/constrain (+ x stepx) 0.0 (width canvas))
        ny (m/constrain (+ y stepy) 0.0 (height canvas))]

    (-> canvas
        (set-color :white)
        (line x y nx ny))

    [nx ny]))

(show-window (black-canvas 640 480) "Random Walk - Levy" draw)

