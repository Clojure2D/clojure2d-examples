(ns GG.P.P-1-1-1-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw rectangles"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [step-x (+ 2 (mouse-x window))
          step-y (+ 2 (mouse-y window))]
      (doseq [^long grid-y (range 0 (height canvas) step-y)
              ^long grid-x (range 0 (width canvas) step-x)]
        (-> canvas
            (set-color (c/from-HSB* (c/color (* grid-x (/ 255.0 (width canvas)))
                                             (* (- (height canvas) grid-y) (/ 255.0 (height canvas)))
                                             255)))
            (rect grid-x grid-y step-x step-y))))))

(show-window (canvas 800 400) "P_1_1_1_01" draw)
