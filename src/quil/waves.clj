;; Original: http://quil.info/sketches/show/example_waves
;; Author: Erik Svedäng

(ns quil.waves
  (:require [clojure2d.core :refer [set-background set-color path show-window canvas]]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const w 500)
(def ^:const h 500)
(def ^:const mult (m/cnorm w 700 200 0.01 0.03))

(def ^:const start (v/vec2 0 h))
(def ^:const end (v/vec2 w h))

(defn calc-y [^double tm ^double x ^double mid ^double amp]
  (+ mid (* (m/sin (+ tm x)) amp)))

(defn wave
  "Calculate path"
  [tm ^double step mid-y amp]
  (conj (cons start
              (mapv #(let [t (* ^long % mult)
                           y (calc-y tm t mid-y amp)]
                       (v/vec2 % y)) (range (- w) (+ step w) step))) end))

(defn draw
  "Draw frames"
  [canvas window ^long fps _]
  (let [t (/ fps ^double (:fps window))]
    (set-background canvas 250 250 250)
    (let [move-down (/ h 5)
          amp (/ h 8)]
      (doseq [y (range move-down (+ amp h) 8)]
        (let [x-step (- (* ^long y 0.8) move-down)
              wv (wave t x-step y amp)]
          (-> canvas
              (set-color 50 230 (+ (* 20 (m/sin t)) 230) 40)
              (path wv false false)
              (set-color :white 250)
              (path wv)))))))

(def window (show-window (canvas w h) "Waves" 30 draw))
