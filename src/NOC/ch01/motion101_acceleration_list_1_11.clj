(ns examples.NOC.ch01.motion101-acceleration-list-1-11
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double topspeed 5.0)
(def ^:const ^int movers-no 20)

(defprotocol MoverProto
  (update-mover [t window])
  (display-mover [t canvas]))

(deftype Mover [^Vec2 position
                velocity]
  MoverProto
  (update-mover [_ window]
    (let [acceleration (-> (mouse-pos window)
                           (v/sub position)
                           (v/normalize)
                           (v/mult 0.2))
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit topspeed))
          ^Vec2 nposition (v/add position velocity)]
      (Mover. nposition
              nvelocity)))
  
  (display-mover [m canvas]
    (-> canvas
        (set-stroke 2.0)
        (filled-with-stroke (c/gray 127 200) :black ellipse (.x position) (.y position) 48 48))
    m))

(defn make-mover
  "Create random Mover"
  [w h]
  (Mover. (Vec2. (r/drand w) (r/drand h))
          (Vec2. 0 0)))

(defn draw
  "Process and display Movers"
  [canvas window _ movers]
  (let [m (or movers (repeatedly movers-no #(make-mover (width window) (height window))))]
    (set-background canvas 255 255 255)
    (mapv #(display-mover (update-mover % window) canvas) m)))

(def window (show-window (canvas 640 360) "Motion 101 Acceleration List (1.11)" draw))

;; it's slower than original version, probably due to overhead in mapv and immutable way of updating
