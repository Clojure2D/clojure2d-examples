(ns example.NOC.introduction.gaussian2
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw on canvas."
  [canvas window framecount state]
  (let [r (r/grand 100 100) ;; setting mean and stdev is built into the library 
        g (r/grand 200 20)
        b (r/grand 0 50)
        xloc (r/grand (* 0.5 (width canvas)) (* 0.1 (width canvas)))
        yloc (r/grand (* 0.5 (width canvas)) (* 0.1 (width canvas)))]

    (-> canvas
        (set-background 0 0 0 1)
        (set-color r g b)
        (ellipse xloc yloc 8 8))))

(show-window (canvas 200 200) "Gaussian2" 120 draw)
