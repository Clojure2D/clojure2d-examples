(ns ex33-interpolations
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 600 500 :high))

(def palette (cycle (first (filter #(> ^double (c/luma (first %)) 150) (repeatedly c/random-palette)))))

(def c1 (first palette))
(def c2 (second palette))
(def c3 (nth palette 2))
(def c4 (nth palette 3))

(defn draw
  "Draw interpolations"
  [c _ ^long fc dir]
  (let [t (m/frac (/ fc 120.0))
        d (if (zero? t) (not dir) dir)
        t' (if d t (- 1.0 t))]

    (comment when (= fc 100) (c2d/save c "results/ex33/interpolations.jpg"))
    
    (-> c
        (c2d/set-background 0 0 0 100)

        (c2d/set-color c1)
        (c2d/ellipse (m/lerp 100 500 t') 100 70 70)
        
        (c2d/set-color c2)
        (c2d/ellipse (m/cos-interpolation 100 500 t') 200 70 70)

        (c2d/set-color c3)
        (c2d/ellipse (m/smooth-interpolation 100 500 t') 300 70 70)

        (c2d/set-color c4)
        (c2d/ellipse (m/quad-interpolation 100 500 t') 400 70 70))

    d))

(def window (c2d/show-window {:canvas cnvs 
                              :window-name "Interpolations"
                              :draw-fn #(draw %1 %2 %3 %4)}))
