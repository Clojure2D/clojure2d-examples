(ns NOC.ch02.mutual-attraction-2-8
  (:require [clojure2d.color :as c]
            [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double g 0.4)

(def ^:const ^int movers-no 20)

(defprotocol MoverProto
  (attract [m v m2])
  (update-and-draw [m other canvas]))

(deftype Mover [^Vec2 position
                ^Vec2 velocity
                ^double mass
                ^long id]
  MoverProto
  (attract [m1 v m2]
    (if (== id (.id ^Mover m2))
      v
      (let [force (v/sub (.position ^Mover m2) position)
            distance (m/constrain (v/mag force) 5.0 25.0)
            strength (/ (* g mass (.mass ^Mover m2)) (m/sq distance))] 
        (v/add v (-> force
                     v/normalize
                     (v/mult strength)
                     (v/div mass))))))
  (update-and-draw [m other canvas]
    (let [acceleration (reduce (partial attract m) (Vec2. 0.0 0.0) other)
          nvelocity (v/add velocity acceleration)
          ^Vec2 nposition (v/add position nvelocity)
          s (* 24.0 mass)]
      
      (-> canvas
          (set-color :black 100)
          (ellipse (.x nposition) (.y nposition) s s)
          (set-stroke 2.0)
          (set-color :black)
          (ellipse (.x nposition) (.y nposition) s s true))

      (Mover. nposition nvelocity mass id))))

(def counter (make-counter))

(defn make-mover
  ""
  [x y m]
  (Mover. (Vec2. x y) (Vec2. 0 0) m (counter)))

(defn draw
  ""
  [canvas window framecount state]
  (let [movers (or state (repeatedly movers-no #(make-mover (r/drand w) (r/drand h) (r/drand 0.1 2))))]

    (set-background canvas :white)

    (mapv #(update-and-draw % movers canvas) movers)))

(def window (show-window (canvas w h) "Mutual attraction 2_8" draw))

