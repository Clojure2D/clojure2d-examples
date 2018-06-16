(ns ex24-stripes
  "Draw stripes" 
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (canvas 600 600))

(def colors (c/gradient :Yxy (c/random-palette)))

(defn draw
  ""
  [canvas window fc state]
  (let [^Vec2 mpos (mouse-pos window) 
        cnt (int (m/cnorm (.x mpos) -1 600 3 100))
        col (m/cnorm (.y mpos) -1 600 0.0 1.0)
        step (/ m/TWO_PI cnt)
        step2 (/ step 2.0)
        vs (reduce #(let [p1x (+ 300 (* 200 (m/cos %2)))
                          p1y (+ 300 (* 200 (m/sin %2)))
                          angle (+ ^double %2 step2)
                          p2x (+ 300 (* 100 (m/cos angle)))
                          p2y (+ 300 (* 100 (m/sin angle)))]
                      (conj %1 (Vec2. p1x p1y) (Vec2. p2x p2y)))
                   []
                   (range 0 (+ step2 m/TWO_PI) step))]
    (set-background canvas 0 0 0)
    (set-color canvas (colors col))
    (triangle-strip canvas vs)))

(def window (show-window cnvs "stripes" draw))

(defmethod key-pressed ["stripes" \space] [_ _]
  (save cnvs "results/ex24/stripes.jpg"))
