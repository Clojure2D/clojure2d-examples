(ns ex33-interpolations
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [fastmath.vector :as v])
  (:import  [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (canvas 600 500 :high))

(def palette (first (filter #(> ^double (c/luma (first %)) 150) (repeatedly c/random-palette))))

(def c1 (first palette))
(def c2 (second palette))
(def c3 (nth palette 2))
(def c4 (nth palette 3))

(defn draw
  "Draw interpolations"
  [c w ^long fc dir]
  (let [t (m/frac (/ fc 120.0))
        d (if (zero? t) (not dir) dir)
        t' (if d t (- 1.0 t))]

    (comment when (= fc 100) (save c "results/ex33/interpolations.jpg"))
    
    (-> c
        (set-background 0 0 0 100)

        (set-color c1)
        (ellipse (m/lerp 100 500 t') 100 70 70)
        
        (set-color c2)
        (ellipse (m/cos-interpolation 100 500 t') 200 70 70)

        (set-color c3)
        (ellipse (m/smooth-interpolation 100 500 t') 300 70 70)

        (set-color c4)
        (ellipse (m/quad-interpolation 100 500 t') 400 70 70))

    d))

(def window (show-window {:canvas cnvs 
                          :window-name "Interpolations"
                          :draw-fn #(draw %1 %2 %3 %4)}))
