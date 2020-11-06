;; Old sketch reimplementation
;; https://www.openprocessing.org/sketch/151044

(ns ex31-wind
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 500 500 :highest))

(def ^:const ^double s (/ m/TWO_PI 320.0)) ;; speed
(def ^:const ^double phase-scale (/ m/PI 400.0)) 

(defn random-c
  "Distortion and alpha - pencil look"
  ^double []
  (* 0.5 ^double (r/drand) (m/qsin (r/drand m/TWO_PI))))

(defn draw
  "Wind algorithm "
  [canvas _ ^long frame state]
  (let [^double a (or state 0.0)]
    (comment when (= frame 200) (binding [c2d/*jpeg-image-quality* 0.9]
                                  (c2d/save cnvs "results/ex31/wind.jpg")))
    (c2d/set-background canvas 226 210 184)
    (dotimes [j 16]
      (dotimes [i 400]
        (let [jj (+ 50 (* j 25))
              ii (+ 50 i)
              step (* (m/sin (* 2.0 m/TWO_PI (r/noise (/ a 40.0)))) (m/sin (* (- 450 ii) phase-scale)))
              swing (->> 50.0
                         (- (* 150.0 (r/noise (+ a (/ ii 200.0))
                                              (+ a (/ jj 300.0))
                                              (/ a 10.0))))
                         (* step)
                         (+ jj))
              dx (random-c)
              dy (random-c)
              x (+ ii dx dx)
              y (+ swing dy dy)] 
          (c2d/set-color canvas 20 20 20 (- 150 (* 150 (m/hypot dx dy))))
          (c2d/ellipse canvas x y 2 2))))
    (+ a s)))

(def window (c2d/show-window cnvs "Wind blows" draw))
