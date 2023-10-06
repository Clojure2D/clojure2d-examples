(ns ex62-eigenvalues
  (:require [fastmath.matrix :as mat]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [clojure2d.pixels :as p]
            [clojure.pprint :as pprint])
  (:import [fastmath.vector Vec2]))

(defmacro emit-random-mat4x4
  "Random matrix coefficients with placeholders `x`, `y`"
  []
  (let [coeffs (repeatedly 16 #(rand-nth ['_x '_y '(- _x) '(- _y) '(+ _x _y) '(- _x _y)
                                          '(* _x _y) '(* _x _x) '(* _y _y)
                                          0 1 -1 2 -2 0.5 -0.5 1.5 -1.5
                                          0 1 -1 2 -2 0.5 -0.5 1.5 -1.5]))]
    (pprint/pprint (mapv vec (partition 4 coeffs)))
    `(mat/mat ~@coeffs)))

(defn eigenvalues
  "Return list of eigenvalues as complex numbers.

  Arguments:

  * `vector-field` - some vector field function used to add some structure
  * `random-input` - random input vector
  * `amount` - influence of the vector field (0 - none, 1 - full)"
  ([vector-field random-input] (eigenvalues vector-field random-input 0.3))
  ([vector-field random-input ^double amount]
   (let [vf (vector-field random-input)
         [^double _x ^double _y] (v/lerp random-input vf amount)] ;; mix input with vector field
     (->>
      #_(mat/mat -0.5 1 1 -1 _x (- _y) -0.5 0 1.5 0 0.5 -1 0 1 -1.5 0)
      (emit-random-mat4x4) ;; create random matrix out of random input / vector field values and some constants
      (mat/eigenvalues)    ;; calculate eigenvalues
      (map-indexed vector) ;; add eigenvalue index, which is used for coloring
      ))))

(defn render
  "Render an image for given vector-field and palette.

  Options:
  * `scale` - eigenvalues scale (default: 100)
  * `amount` - amount of between number point and vector vield value"
  ([vector-field palette] (render vector-field palette nil))
  ([vector-field palette {:keys [^double scale ^double amount ^long points]
                          :or {scale 100.0 amount 0.3 points 500000}}]
   (let [buffer (p/renderer 800 800)] ;; log-density buffer
     (doseq [in (repeatedly points #(v/generate-vec2 r/grand)) ;; generate random points from gaussian
             [id ^Vec2 point] (eigenvalues vector-field in amount) ;; get eigenvalues
             :when (not (m/zero? (.y point))) ;; remove pure real values
             :let [^Vec2 point (v/shift (v/mult point scale) 400.0) ;; translate for the screen
                   col (palette id)]] ;; get color from palette 
       (p/set-color! buffer (.y point) (.x point) col)) ;; add point, rotating 90deg, imaginary horizontal
     buffer)))

(defn render-paralelly
  "Run n rendereres in parallel and combine the result"
  ([vector-field palette {:keys [cpus]
                          :or {cpus c2d/available-cores}
                          :as options}]
   (let [renderers (doall (repeatedly cpus #(future (render (or vector-field identity) palette options))))]
     (reduce p/merge-renderers (map deref renderers)))))

;; build a vector field as a random combination of other fields
(def field (let [random-field-config (f/random-configuration 1)
               field (f/combine random-field-config)]
           (pprint/pprint random-field-config)
           field))

(def palette (c/resample (c/palette) 4)) ;; ensure palette contains 4 values by resampling

(utils/show-palette palette)

(def result (let [vector-field field
                #_#_vector-field identity
                buffer (render-paralelly vector-field palette {:scale 100 :amount 0.3 :points 50000})
                result (-> buffer
                           (p/to-pixels {:brightness 1.2 :saturation 1.4 ;; make some adjustments
                                         :gamma-alpha 1.2 :contrast 1.1}))]
            (utils/show-image result)
            result))

(comment
  (c2d/save result (c2d/next-filename "results/ex62/" ".jpg")))
