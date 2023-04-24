(ns ex49-archimedean-spiral
  "Generates an archimedean spiral with an oscillating radius"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

;; be sure everything is fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const w 800)
(def ^:const h 800)

(def ^:const x0 400)
(def ^:const y0 400)

(def ^:const tStep 0.01)

(def ^:const tAngleScale m/HALF_PI)
(def ^:const tRadiusScale 10.0)

(def ^:const ampFactor 0.3)
(def ^:const periodFactor 16)

(defn draw [canvas _ ^double frame _]
  (let [t1 (/ frame 60.0)]
    (-> canvas
        (c2d/set-background :black 50)
        (c2d/set-color :white 80))
    (let [p (for [^double t (range 0.0 t1 tStep)]
              (let [theta (* t tAngleScale)
                    r (* (+ 1 (* ampFactor (m/sin (* periodFactor theta)))) (* t tRadiusScale))]
                [(+ x0 (* (m/cos theta) r)) (+ y0 (* (m/sin theta) r))]))]
      (c2d/path canvas p))))

(def window (c2d/show-window {:canvas (c2d/canvas w h :highest)
                            :window-name "Archimedean spiral with an oscillating radius"
                            :draw-fn draw}))
