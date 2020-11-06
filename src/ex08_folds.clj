(ns ex08-folds
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :as vr]
            [clojure2d.extra.overlays :as o]
            [clojure.pprint :refer [pprint]])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 540)
(def ^:const ^long h 540)

(def ^:const ^double x1 -2.0)
(def ^:const ^double y1 -2.0)
(def ^:const ^double x2 2.0)
(def ^:const ^double y2 2.0)
(def ^:const ^double step (/ (- x2 x1) (* 1.8 w)))

(def ^:const ^double x1- (dec x1))
(def ^:const ^double y1- (dec y1))
(def ^:const ^double x2+ (inc x2))
(def ^:const ^double y2+ (inc y2))
(def ^:const ^long w- (dec w))
(def ^:const ^long h- (dec h))

(def ^:const ^double fscale 0.7)

(def s60 (future (o/spots-overlay w h {:alpha 60 :intensities [60 120 180]})))
(def n60 (future (o/noise-overlay w h {:alpha 60})))

(defn make-me
  ""
  [canvas window]
  (let [field-config (vr/random-configuration)
        field (vr/combine field-config)
        ;; field (vr/make-variation :shreadrad)
        ] 

    (pprint field-config)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (v/mult (v/sin (v/mult (field (Vec2. x y)) fscale)) 2.7)
              xx (m/norm (+ (.x vv) (r/grand 0.0012)) x1- x2+ 0.0 w)
              yy (m/norm (+ (.y vv) (r/grand 0.0012)) y1- y2+ 0.0 h)]
          (c2d/point canvas xx yy))
        
        (when (and (c2d/window-active? window) (< x x2)) (recur (+ x step))))
      (when (and (c2d/window-active? window) (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (c2d/with-canvas-> canvas
    (c2d/set-background 255 250 245)
    (c2d/set-color 35 35 35 16)
    (make-me disp)
    (c2d/image (o/render-noise (c2d/get-image canvas) @n60))
    (c2d/image (o/render-spots (c2d/get-image canvas) @s60)))
  :done)

(defn example-08
  ""
  []
  (let [cnvs (c2d/canvas w h)
        window (c2d/show-window cnvs "folds" 15 nil)]

    (defmethod c2d/key-pressed ["folds" \space] [_ _]
      (c2d/save cnvs (c2d/next-filename "results/ex08/" ".jpg")))

    [cnvs window]))

(draw-folds (example-08))

;; [[../results/ex08/1FABF63A_000013.jpg]]

