(ns examples.NOC.introduction.randomwalktendstoright-I-3
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 (width canvas))
                                         (* 0.5 (height canvas))])
        choice ^double (r/drand)
        [^double nx ^double ny] (condp > choice
                                  0.4 [(inc x) y]
                                  0.6 [(dec x) y]
                                  0.8 [x (inc y)]
                                  [x (dec y)])
        nx (m/constrain nx 0 (width canvas))
        ny (m/constrain ny 0 (height canvas))]

    (-> canvas
        (set-stroke 2.0)
        (set-color :black)
        (point nx ny))

    [nx ny]))

(let [canvas (canvas 640 360)]

  (with-canvas-> canvas
    (set-background :white))

  (show-window canvas "Random Walk - tends to right I_3" draw))
