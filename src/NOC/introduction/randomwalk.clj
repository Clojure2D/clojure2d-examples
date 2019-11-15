(ns examples.NOC.introduction.randomwalk
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 (width canvas))
                                         (* 0.5 (height canvas))])
        vx (r/drand -2.0 2.0)
        vy (r/drand -2.0 2.0)
        nx (m/constrain (+ x vx) 0.0 (width canvas))
        ny (m/constrain (+ y vy) 0.0 (height canvas))]

    (-> canvas
        (set-background :white)
        (set-color 175 175 175)
        (crect nx ny 40 40)
        (set-color 0 0 0)
        (crect nx ny 40 40 true))

    [nx ny]))

(show-window (canvas 400 400) "Random Walk" 30 draw)
