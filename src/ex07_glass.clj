(ns ex07-glass
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :as vr]
            [clojure2d.color :as c]
            [clojure.pprint :refer [pprint]])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw-glass
  ""
  [canvas window ^long width ^long height]
  (binding [vr/*skip-random-fields* true]
    (let [hw (long (/ height 2))
          ww (long (/ width 2))
          field-config (vr/random-configuration)
          field (vr/combine field-config)]
      (pprint field-config)
      (loop [x (int 0)]
        (loop [y (int 0)]
          (let [xt (/ (- x ww) 120.0)
                yt (/ (- y hw) 120.0)
                ^Vec2 n (field (Vec2. xt yt))
                n1 (r/simplex (.x n) (.y n))
                n2 (r/simplex (.y n) (.x n) 0.3)
                v1 (m/constrain n1 0 1)
                v2 (m/constrain n2 0 1)]
            (c2d/set-color canvas (c/color (* 255.0 v1 v1) (* 255.0 v1 v2) (* 255.0 v2)))
            (c2d/rect canvas x y 1 1))

          (when (and (c2d/window-active? window) (< y height)) (recur (inc y))))
        (when (and (c2d/window-active? window) (< x width)) (recur (inc x)))))))

(let [cnvs (c2d/canvas 800 800)
      window (c2d/show-window cnvs "glass" 15 nil)]

  (defmethod c2d/key-pressed ["glass" \space] [_ _]
    (c2d/save cnvs (c2d/next-filename "results/ex07/" ".jpg")))


  (c2d/with-canvas-> cnvs
    (c2d/set-background 200 200 210)
    (draw-glass window 800 800)))
