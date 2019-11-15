(ns NOC.ch06.seek-trail-6-1
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import fastmath.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double maxspeed 4.0)
(def ^:const ^double maxforce 0.1)
(def ^:const ^double r 6.0)
(def ^:const ^double r2 (+ r r))

(defn seek
  ""
  [target position velocity]
  (let [acceleration (-> target
                         (v/sub position)
                         (v/set-mag maxspeed)
                         (v/sub velocity)
                         (v/limit maxforce))
        nvelocity (-> velocity
                      (v/add acceleration)
                      (v/limit maxspeed))]
    [(v/add position nvelocity) nvelocity]))

(defn draw
  ""
  [canvas window _ state]
  (let [[position velocity history] (or state [(Vec2. 400.0 100.0) (Vec2. 0.0 -2.0) clojure.lang.PersistentQueue/EMPTY])
        [^Vec2 nposition nvelocity] (seek (mouse-pos window) position velocity)
        theta (+ m/HALF_PI (v/heading velocity))
        nhistory (conj history nposition)
        nhistory (if (== (count nhistory) 100) (pop nhistory) nhistory)]

    (-> canvas
        (set-background :white)
        (set-color 200 200 200)
        (ellipse (mouse-x window) (mouse-y window) 48 48)
        (set-color :black)
        (set-stroke 2.0)
        (ellipse (mouse-x window) (mouse-y window) 48 48 true)
        (set-stroke 1.0)
        (set-color :black) 
        (path nhistory)
        (set-color 127 0 0)
        (push-matrix)
        (translate (.x nposition) (.y nposition))
        (rotate theta)
        (triangle 0 (- r2) (- r) r2 r r2)
        (pop-matrix))

    [nposition nvelocity nhistory]))

(def window (show-window (canvas 800 200) "Seek with trail 6_1" draw))
