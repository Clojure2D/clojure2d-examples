(ns examples.NOC.introduction.noisedistribution
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  ""
  [canvas _ ^long framecount state]
  (let [vals (or state (repeat (width canvas) 0.0))
        xoff (* 0.01 framecount)
        n (r/simplex xoff)
        index (int (* n (width canvas)))
        nvals (vec (map-indexed #(if (== ^int %1 index) (inc ^double %2) %2) vals)) ;; increase hit index
        mx (v/mx nvals)
        normalized (if (> mx (height canvas))
                     (mapv #(* (height canvas) (/ ^double % mx)) nvals)
                     nvals)]

    (set-background canvas 100 100 100)
    (set-color canvas :white)

    ;; draw lines
    (dotimes [x (width canvas)]
      (line canvas x (height canvas) x (- (height canvas) ^double (normalized x))))

    nvals))

(def window (show-window (canvas 300 200) "Noise distribution" 200 draw))

