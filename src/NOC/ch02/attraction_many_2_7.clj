(ns NOC.ch02.attraction-many-2-7
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

(def ^:const ^int movers-no 10)

(def ^:const ^double amass 20.0)

(defprotocol MoverProto
  (update-and-draw [t canvas window]))
(deftype Mover [^Vec2 position
                ^Vec2 velocity
                ^double mass]
  MoverProto
  (update-and-draw [_ canvas window]
    (let [att-pos (get-state window)
          force (v/sub att-pos position)
          d (m/constrain (v/mag force) 5.0 25.0)
          strength (/ (* amass mass) (m/sq d))
          nvelocity (v/add velocity (-> force
                                        v/normalize
                                        (v/mult strength)
                                        (v/div mass)))
          ^Vec2 nposition (v/add position nvelocity)
          s (* 25.0 mass)]

      (-> canvas
          (set-color (c/gray 127))
          (ellipse (.x nposition) (.y nposition) s s)
          (set-stroke 2.0)
          (set-color :black)
          (ellipse (.x nposition) (.y nposition) s s true))
      
      (Mover. nposition nvelocity mass))))

(defn make-mover
  ""
  [x y m]
  (Mover. (Vec2. x y) (Vec2. 1 0) m))

(defn draw
  ""
  [canvas window framecount state]
  (let [movers (or state (repeatedly movers-no #(make-mover (r/drand w) (r/drand h) (r/drand 0.1 2))))
        ^Vec2 pos (get-state window)]

    (-> canvas
        (set-background :white)
        (set-stroke 4.0)
        (filled-with-stroke (if (< (v/dist (mouse-pos window) pos) amass)
                              (c/gray 100)
                              (c/gray 175 200)) :black
                            ellipse (.x pos) (.y pos) (* 2.0 amass) (* 2.0 amass)))

    (mapv #(update-and-draw % canvas window) movers)))

(def window (show-window {:canvas (canvas w h)
                          :window-name "Attraction many 2_7"
                          :draw-fn draw
                          :state (Vec2. (/ w 2) (/ h 2))}))

(defmethod mouse-event ["Attraction many 2_7" :mouse-dragged] [e _]
  (mouse-pos e))
