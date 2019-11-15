(ns examples.NOC.introduction.gaussian-I-4
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  ""
  [canvas _ _ _]
  (let [xloc (r/grand (* 0.5 (width canvas)) 60)]

    (-> canvas
        (set-color :black 10)
        (ellipse xloc (* 0.5 (height canvas)) 16 16))))

(let [canvas (canvas 640 360)]

  (with-canvas-> canvas
    (set-background :white))
  
  (show-window canvas "Random Walk - Gaussian" draw))
