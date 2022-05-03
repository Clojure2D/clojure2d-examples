(ns rt4.the-next-week.ch05a.texture
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [rt4.color :as color]
            [rt4.the-next-week.ch05a.perlin :as perlin]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol TextureProto
  (value [texture u v p]))

(defrecord SolidColor [color]
  TextureProto
  (value [_ _ _ _] color))

(defn solid-color
  ([color] (->SolidColor color))
  ([red green blue] (->SolidColor (v/vec3 red green blue))))

;;

(defrecord CheckerTexture [^double inv-scale even odd]
  TextureProto
  (value [_ u v p]
    (let [xyz (-> p (v/mult inv-scale) v/floor v/sum int)]
      (if (even? xyz) (value even u v p) (value odd u v p)))))

(defn checker-texture
  [^double scale even odd]
  (->CheckerTexture (/ scale) even odd))

(defn color-checker-texture
  [^double scale c1 c2]
  (checker-texture scale (solid-color c1) (solid-color c2)))

;;

(defrecord ImageTexture [image]
  TextureProto
  (value [_ u v _]
    (let [u (m/constrain ^double u 0.0 1.0)
          v (- 1.0 (m/constrain ^double v 0.0 1.0))
          i (* u (c2d/width image))
          j (* v (c2d/height image))]
      (color/<-color (p/get-color image i j)))))

(defn image-texture [filename]
  (let [image (p/load-pixels filename)]
    (if-not (pos? (c2d/height image))
      (solid-color (v/vec3 0.0 1.0 1.0))
      (->ImageTexture image))))

;;

(def ^:private one (v/vec3 1.0 1.0 1.0))

(defrecord NoiseTexture [noise]
  TextureProto
  (value [_ _ _ p]
    (v/mult one (perlin/noise noise p))))

(defn noise-texture []
  (->NoiseTexture (perlin/perlin)))
