(ns ex06-caustic
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :as vr]
            [clojure.pprint :refer [pprint]])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double min-range -2.0)
(def ^:const ^double max-range 2.0)
(def ^:const ^long tilt-scale 4)
(def ^:const ^double delta-scale 0.5)
(def ^:const ^long shift-scale 30)

(defn draw-caustic
  ""
  [canvas window ^long width ^long height]
  (binding [vr/*skip-random-fields* true]
    (let [hw (long (/ width 2))
          hh (long (/ height 2))
          d shift-scale
          d2 (* d 2)
          d2- (- d2)
          field-config (vr/random-configuration)
          field (vr/combine field-config)]
      (pprint field-config)
      (loop [x (double d2-)]
        (loop [y (double d2-)]
          (let [hx (m/norm (- x hw) (- hw) hw min-range max-range)
                hy (m/norm (- y hh) (- hh) hh min-range max-range)
                hhx (* tilt-scale hx)
                hhy (* tilt-scale hy)
                delta (* delta-scale ^double (m/norm (r/noise hx hy) 0 1 -1 1))
                ^Vec2 v1 (field (Vec2. (- hhx delta) (- hhy delta)))
                ^Vec2 v2 (field (Vec2. (+ hhx delta) (+ hhy delta)))
                dx (* d (- (.x v1) (.x v2)))
                dy (* d (- (.y v1) (.y v2)))]
            (c2d/rect canvas (+ dx x) (+ dy y) 1 1))
          (when (and (c2d/window-active? window) (< y (+ d2 height))) (recur (+ y 0.3))))
        (when (and (c2d/window-active? window) (< x (+ d2 width))) (recur (+ x 0.3))))))
  canvas)

(let [cnvs (c2d/canvas 800 800)
      window (c2d/show-window cnvs "caustic" 15 nil)]

  (defmethod c2d/key-pressed ["caustic" \space] [_ _]
    (c2d/save cnvs (c2d/next-filename "results/ex06/" ".jpg")))

  (c2d/with-canvas-> cnvs
    (c2d/set-color 10 20 40 30)
    (c2d/set-background 200 200 210)
    (draw-caustic window 800 800)))
