(ns examples.NOC.ch04.particlesystemsmoke-4-8
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

;; change to true for ellipse version (b)
(def ^:const version-b false)

(def img (p/load-pixels "src/NOC/ch04/texture.png"))
(def filtered-images (mapv #(-> (p/tint (c/color 255 255 255 (* ^double % 2.5)))
                                (p/filter-channels true img)
                                (get-image)) (range 51)))

(def ^:const img-hw (* (width img) 0.5))
(def ^:const img-hh (* (height img) 0.5))

(defprotocol ParticleProto
  (run [p canvas wind]))

(deftype Particle [pos vel ^int lifespan]
  ParticleProto
  (run [_ canvas wind]
    (let [nlifespan (dec lifespan)
          nvel (v/add vel wind)
          ^Vec2 npos (v/add pos nvel)]

      (if version-b
        (-> canvas
            (set-color :white (max 0 nlifespan))
            (ellipse (.x npos) (.y npos) (width img) (height img)))
        (image canvas (filtered-images (max 0 nlifespan)) (- (.x npos) img-hw) (- (.y npos) img-hh)))
      
      (Particle. npos nvel nlifespan))))

(defn make-particle
  "Create random Particle"
  []
  (Particle. (Vec2. (* w 0.5) (- h 75))
             (Vec2. (r/grand 0.3) (dec (r/grand 0.3)))
             50))

(defn draw
  "Draw arrow and smoke"
  [canvas window _ state]
  (let [particles (or state [(make-particle)])
        dx (m/norm (mouse-x window) 0 w -0.2 0.2)
        wind (Vec2. dx 0.0)
        len (* 500.0 (v/mag wind))]

    (-> canvas ;; arrow
        (set-background :black)
        (set-color :white)
        (push-matrix)
        (translate (* w 0.5) 50)
        (rotate (v/heading wind))
        (line 0 0 len 0)
        (line len 0 (- len 4) 2)
        (line len 0 (- len 4) -2)
        (pop-matrix))
    
    (mapv #(run % canvas wind) (filter #(pos? (.lifespan ^Particle %)) (conj particles (make-particle) (make-particle))))))

(def window (show-window (canvas w h) "Particle system smoke 4_8" draw))
