(ns rt4.the-rest-of-your-life.ch06a.texture
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [rt4.color :as color]
            [rt4.the-rest-of-your-life.ch06a.perlin :as perlin]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol TextureProto
  (value [texture u v p]))

;; a color is also a texture (same as SolidTexture)
(extend-protocol TextureProto
  Vec3
  (value [this _ _ _] this))

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

(def fbm-noise (r/fbm-noise {:octaves 7 :normalize? false}))

(defrecord NoiseTexture [noise ^double scale]
  TextureProto
  (value [_ _ _ p]
    #_(v/mult one (perlin/turb noise (v/mult p scale)))
    ;; using fastmath fbm-noise, which is much much faster
    ;; also using code from version 3.x
    (let [^fastmath.vector.Vec3 p p]
      (v/mult one (* 0.5 (inc (m/sin (+ (* scale (.z p))
                                        (* 10.0 (m/abs ^double (fbm-noise (.x p) (.y p) (.z p))))))))))))

(defn noise-texture [^double scale]
  (->NoiseTexture (perlin/perlin) scale))
