(ns examples.NOC.introduction.noise1d
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as n]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double xincrement 0.005)

(defn draw
  ""
  [canvas _ _ state]
  (let [^double xoff (or state 0.0)
        n (* (width canvas) (n/noise xoff))]

    (-> canvas
        (set-background 0 0 0 10)
        (set-color 200 200 200)
        (ellipse n (* 0.5 (height canvas)) 16 16))

    (+ xoff xincrement)))

(show-window (canvas 200 200) "Noise 1D" draw)

;; clojure2d noise has more octaves than Processing noise, some scalling is needed
