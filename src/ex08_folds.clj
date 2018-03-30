(ns ex08-folds
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.extra.variations :as vr]
            [clojure2d.extra.overlays :refer :all]
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

(def s60 (future (make-spots w h {:alpha 60 :intensities [60 120 180]})))
(def n60 (future (make-noise w h {:alpha 60})))

(defn make-me
  ""
  [canvas window]
  (let [field-config (vr/make-random-configuration)
        field (vr/make-combination field-config)
        ;; field (vr/make-variation :shreadrad)
        ] 

    (pprint field-config)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (v/mult (v/applyf (v/mult (field (Vec2. x y)) fscale) #(m/sin %)) 2.7)
              xx (m/norm (+ (.x vv) (r/grand 0.0012)) x1- x2+ 0.0 w)
              yy (m/norm (+ (.y vv) (r/grand 0.0012)) y1- y2+ 0.0 h)]
          (point canvas xx yy))
        
        (when (and (window-active? window) (< x x2)) (recur (+ x step))))
      (when (and (window-active? window) (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas-> canvas
    (set-background 255 250 245)
    (set-color 35 35 35 16)
    (make-me disp)
    (image (render-noise (get-image canvas) @n60))
    (image (render-spots (get-image canvas) @s60)))
  :done)

(defn example-08
  ""
  []
  (let [cnvs (canvas w h)
        window (show-window cnvs "folds" 15 nil)]

    (defmethod key-pressed ["folds" \space] [_ _]
      (save cnvs (next-filename "results/ex08/" ".jpg")))

    [cnvs window]))

(draw-folds (example-08))

;; [[../results/ex08/1FABF63A_000013.jpg]]

