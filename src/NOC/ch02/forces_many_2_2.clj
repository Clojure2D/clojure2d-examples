(ns examples.NOC.ch02.forces-many-2-2
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^int number-of-movers 20)

(def wind (Vec2. 0.01 0.0))
(def gravity (Vec2. 0.0 0.1))

(deftype Mover [position velocity ^double mass])

(defn make-mover
  "Create Mover"
  []
  (->Mover (Vec2. 0.0 0.0)
           (Vec2. 0.0 0.0)
           (r/drand 0.1 4.0)))

(defn apply-force
  "Apply force"
  [a f mass]
  (v/add a (v/div f mass)))

(defn check-edges
  "Check window boundaries"
  [^Vec2 velocity ^Vec2 pos]
  (let [mx (if (< -0.01 (.x pos) w) 1.0 -1.0)
        my (if (< -0.01 (.y pos) h) 1.0 -1.0)]
    [(v/emult velocity (Vec2. mx my))
     (Vec2. (m/constrain (.x pos) 0 w)
            (m/constrain (.y pos) 0 h))]))

(defn move-mover
  "Move mover"
  [^Mover m]
  (let [acc (-> (Vec2. 0.0 0.0)
                (apply-force wind (.mass m))
                (apply-force gravity (.mass m)))
        vel (v/add (.velocity m) acc)
        pos (v/add (.position m) vel)
        [new-vel new-pos] (check-edges vel pos)]
    (->Mover new-pos new-vel (.mass m))))

(defn draw-and-move
  "Draw mover, move and return new one."
  [canvas ^Mover m]
  (let [size (* 16.0 ^double (.mass m))]
    (-> canvas
        (set-color :black 127)
        (ellipse (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size false)
        (set-stroke 2)
        (set-color :black)
        (ellipse (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size true))
    (move-mover m)))

(defn draw
  "Draw movers on canvas"
  [canvas _ _ last-m]
  (set-background canvas :white)
  (let [m (or last-m (repeatedly number-of-movers make-mover))]
    (mapv (partial draw-and-move canvas) m)))

(def window (show-window (canvas w h) "NOC_2_2_forces_many" draw))

