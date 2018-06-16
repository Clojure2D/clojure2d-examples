(ns examples.NOC.introduction.multipleprobability
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double p1 0.05)
(def ^:const ^double p2 (+ 0.8 p1))

(defn draw
  ""
  [canvas _ _ state]
  (let [^Vec2 v (or state (Vec2. 0.0 0.0))
        c (condp > (r/drand)
            p1 255
            p2 150
            0)
        nx (m/wrap 0 (width canvas) (+ 10.0 (.x v)))
        ny (if (zero? nx)
             (m/wrap 0 (height canvas) (+ 10.0 (.y v)))
             (.y v))]

    (-> canvas
        (set-background :black 1)
        (set-color c c c)
        (rect (.x v) (.y v) 10 10)
        (set-color 200 200 200)
        (rect (.x v) (.y v) 10 10 true))

    (Vec2. nx ny)))

(show-window (canvas 200 200) "Multiple probability" draw)
