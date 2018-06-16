(ns examples.NOC.introduction.noisewalk-many
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; no deftype approach

(def nstep (v/vec2 0.001 0.001))
(def noise-seed (r/irand))
(def noise-conf (assoc (r/random-noise-cfg) :seed noise-seed))

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [walkers (or state (repeatedly 10 #(vector (v/vec2 0 0)
                                                  (v/generate-vec2 (partial r/drand 1000)))))
        octaves (int (m/cnorm (mouse-x window) 0 (width window) 1 8))
        noise (r/fbm-noise (assoc noise-conf :octaves octaves))
        total (m/constrain (/ framecount 30) 1 10)
        
        result (map (fn [[_ ^Vec2 noff]] 
                      (let [nx (m/norm (noise (.x noff)) 0.0 1.0 0 (width canvas))
                            ny (m/norm (noise (.y noff)) 0.0 1.0 0 (height canvas))]
                        [(Vec2. nx ny) (v/add noff nstep)])) walkers)]

    (set-background canvas :white)

    (dorun (take total (map #(let [[^Vec2 position] %] 
                               (-> canvas
                                   (set-color :gray)
                                   (ellipse (.x position) (.y position) 48 48)
                                   (set-stroke 2.0)
                                   (set-color :black)
                                   (ellipse (.x position) (.y position) 48 48 true))) result)))

    result))

(def window (show-window (canvas 600 400) "Noise walk - many" draw))
