(ns ex60-metal-sheets
  (:require [fastmath.fields :as f]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as u]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)
(m/use-primitive-operators)

(def ^:const SIZE 800)

(def grad (c/gradient [:black :white]))

(def window (binding [f/*skip-random-fields* (r/brand 0.8)]
            (let [buff (p/renderer SIZE SIZE :sinc 1.51 1.95)
                  field (f/random-field (r/irand 6))]
              (doseq [^long x (range SIZE)
                      ^long y (range SIZE)
                      [^double sx ^double sy] (take 10 (r/jittered-sequence-generator :r2 2))
                      :let [nx (+ x sx)
                            ny (+ y sy)
                            xx (m/norm nx 0.0 SIZE -3.5 3.5)
                            yy (m/norm ny 0.0 SIZE -3.5 3.5)
                            in (v/vec2 xx yy)
                            v (field in)
                            c (grad (/ (v/angle-between v in) m/PI))]]
                (p/set-color! buff nx ny c))
              (u/show-image buff))))

(comment
  (c2d/save (c2d/to-image window) (c2d/next-filename "results/ex60/" ".jpg"))
  )
