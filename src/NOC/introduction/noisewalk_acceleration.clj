(ns examples.NOC.introduction.noisewalk-acceleration
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; no deftype approach

(def nstep (v/vec2 0.01 0.01))

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [[position velocity noff history] (or state [(v/vec2 (* 0.5 (width canvas)) (* 0.5 (height canvas))) ;; position
                                                    (v/vec2 0 0) ;; velocity
                                                    (v/generate-vec2 (partial r/drand 1000)) ;; noise field position
                                                    clojure.lang.PersistentQueue/EMPTY]) ;; history
        ^Vec2 nnoff (v/add noff nstep)
        nvelocity (-> (v/vec2 (m/norm (r/noise (.x nnoff)) 0 1 -1 1)
                              (m/norm (r/noise (.y nnoff)) 0 1 -1 1))
                      (v/mult 0.1)
                      (v/add velocity)
                      (v/limit 1.0))
        ^Vec2 nposition (v/add position nvelocity)
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

    [nposition nvelocity nnoff nhistory]))

(def window (show-window (canvas 640 360) "Noise walk - acceleration" draw))
