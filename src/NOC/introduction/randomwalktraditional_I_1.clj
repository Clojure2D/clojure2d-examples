(ns examples.NOC.introduction.randomwalktraditional_I_1
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
        choice (int (r/irand 4))
        [^double nx ^double ny] (case choice
                                  0 [(inc x) y]
                                  1 [(dec x) y]
                                  2 [x (inc y)]
                                  3 [x (dec y)])
        nx (m/constrain nx 0 (width canvas))
        ny (m/constrain ny 0 (height canvas))]

    (-> canvas
        (set-color :black)
        (point nx ny))

    [nx ny]))

(let [canvas (canvas 640 360)]

  (with-canvas-> canvas
    (set-background :white))

  (show-window canvas "Random Walk - Traditional I_1" draw))
