(ns examples.NOC.introduction.noise2d
  (:require [clojure2d.color :as c]
            [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def cnvs (canvas 640 360))
(def window (show-window cnvs "Noise 2d"))

(let [pixels (p/to-pixels cnvs)]

  (dotimes [x (width cnvs)]
    (dotimes [y (height cnvs)]
      (let [xoff (m/norm x 0 (width cnvs) 0 (* 0.02 (width cnvs)))
            yoff (m/norm y 0 (height cnvs) 0 (* 0.02 (height cnvs)))
            b (* 255.0 (r/noise xoff yoff))]

        (p/set-color! pixels x y (c/gray b)))))

  (p/set-canvas-pixels! cnvs pixels))
