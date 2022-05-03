(ns rt4.the-next-week.ch04b.texture
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

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
