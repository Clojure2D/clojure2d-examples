;; Random or uniform generators
;; See section "Random Vectors": http://commons.apache.org/proper/commons-math/userguide/random.html

(ns ex30-generators
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def cnvs (c2d/canvas 900 900 :highest))
(def window (c2d/show-window cnvs "Generators" 5 nil))

(defmethod c2d/key-pressed ["Generators" \space] [_ _]
  (binding [c2d/*jpeg-image-quality* 0.9]
    (c2d/save cnvs "results/ex30/result.jpg")))

(defn draw-random-rect
  "Draw random rectangle from dots from random sequence"
  [canvas generator posx posy scale num]
  (let [transformed (map #(v/mult % scale) generator)]
    (doseq [^Vec2 v (take num transformed)]
      (c2d/ellipse canvas (+ ^double posx (.x v)) (+ ^double posy (.y v)) 4 4)))
  canvas)

(c2d/with-canvas-> cnvs
  (c2d/set-background 10 10 10)
  (c2d/set-color 255 100 100 200)
  (draw-random-rect (r/sequence-generator :halton 2) 50 50 225 1000) ;; upper left
  (draw-random-rect (r/sequence-generator :sobol 2) 325 50 225 1000) ;; upper middle
  (draw-random-rect (r/sequence-generator :r2 2) 600 50 225 1000) ;; upper right
  (c2d/set-color 100 100 255 200)
  (draw-random-rect (r/jittered-sequence-generator :r2 2 0.4) 600 325 225 1000) ;; middle right
  (draw-random-rect (r/jittered-sequence-generator :r2 2 1.0) 600 625 225 1000) ;; right right
  (c2d/set-color 0 255 0 150)
  (draw-random-rect (r/sequence-generator :gaussian 2) 300 450 (/ 112.5 2) 2000) ;; middle
  (c2d/set-color 200 200 200 200)
  (draw-random-rect (r/sequence-generator :sphere 2) 162.5 737.5 112.5 150) ;; bottom left
  (draw-random-rect (r/sequence-generator :default 2) 325 625 225 1000) ;; bottom middle
  (c2d/set-color :white)
  (c2d/translate 0 -10)
  (c2d/text "Halton" 50 50)
  (c2d/text "Sobol" 325 50)
  (c2d/text "R2" 600 50)
  (c2d/text "R2 jittered (lambda=0.4)" 600 325)
  (c2d/text "R2 jittered (lambda=1.0)" 600 625)
  (c2d/text "Gaussian" 50 450)
  (c2d/text "2d Sphere" 162.5 737.5)
  (c2d/text "Uniform random" 325 625)) 
