(ns ex49-archimedean_spiral
  "Generates an archimedean spiral"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m])
    (:import [fastmath.vector Vec2]))

;; be sure everything is fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long fps 60)

(def ^:const ^long w 800)
(def ^:const ^long h 800)

(def ^:const ^double x0 400)
(def ^:const ^double y0 400)

(def ^:const ^double tStep 0.5)

(def ^:const ^double tAngleScale (/ m/PI 2))
(def ^:const ^double tRadiusScale 10)

(defn draw [canvas window ^long frame _]
    (let [field (get-state window)
          t     ^double (/ frame fps)]
      (-> canvas
          (set-background :black 50)
          (set-color :white 80))
      (let [p (for [^double theta (range 0 t tStep)] (Vec2.
                  (+ x0 (* (m/cos (* theta ^double tAngleScale)) (* theta tRadiusScale)))
                  (+ y0 (* (m/sin (* theta ^double tAngleScale)) (* theta tRadiusScale)))))]
        (path-bezier canvas p false true))))

(def window (show-window {:canvas (canvas w h)
                          :window-name "Archimedean spiral"
                          :draw-fn draw
                          :fps fps}))