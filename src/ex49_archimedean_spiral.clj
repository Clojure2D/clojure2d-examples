(ns ex49-archimedean_spiral
  "Generates an archimedean spiral with an oscillating radius"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

;; be sure everything is fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 800)
(def ^:const ^long h 800)

(def ^:const ^double x0 400)
(def ^:const ^double y0 400)

(def ^:const ^double tStep 0.01)

(def ^:const ^double tAngleScale m/HALF_PI)
(def ^:const ^double tRadiusScale 10.0)

(def ^:const ^double ampFactor 0.3)
(def ^:const ^double periodFactor 16)

(defn draw [canvas window ^double frame _]
  (let [t1 (/ frame 60.0)]
    (-> canvas
        (set-background :black 50)
        (set-color :white 80))
    (let [p (for [^double t (range 0.0 t1 tStep)]
              (let [theta (* t tAngleScale)
                    r (* (+ 1 (* ampFactor (m/sin (* periodFactor theta)))) (* t tRadiusScale))]
                [(+ x0 (* (m/cos theta) r)) (+ y0 (* (m/sin theta) r))]))]
      (path canvas p))))

(def window (show-window {:canvas (canvas w h :highest)
                          :window-name "Archimedean spiral with an oscillating radius"
                          :draw-fn draw}))
