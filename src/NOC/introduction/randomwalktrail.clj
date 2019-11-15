(ns examples.NOC.introduction.randomwalktrail
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; no deftype approach

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [[position history] (or state [(v/vec2 (* 0.5 (width canvas)) (* 0.5 (height canvas))) ;; position
                                      clojure.lang.PersistentQueue/EMPTY]) ;; history
        velocity (v/generate-vec2 (partial r/drand -2.0 2.0))
        ^Vec2 nposition (v/add position velocity)
        ^Vec2 nposition (v/vec2 (m/constrain (.x nposition) 8 (- (width window) 9))
                                (m/constrain (.y nposition) 8 (- (height window) 9)))
        nhistory (conj history nposition)
        nhistory (if (== (count nhistory) 1001) (pop nhistory) nhistory)]

    (-> canvas
        (set-background :white)
        (set-color 175 175 175)
        (crect (.x nposition) (.y nposition) 16 16)
        (set-color 0 0 0)
        (crect (.x nposition) (.y nposition) 16 16 true)
        (path nhistory))

    [nposition nhistory]))

(def window (show-window (canvas 400 400) "Random walk - trail" 30 draw))
