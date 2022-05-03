(ns rt4.color
  (:require [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3 Vec4]))

(defn linear-to-gamma
  ^double [^double v]
  (m/sqrt v))

(defn ->color
  "Convert to clojure2d color, optionally divide by a given constant"
  ([^double r ^double g ^double b] (->color (Vec3. r g b)))
  ([in] (-> in
            (v/fmap linear-to-gamma)
            c/scale-up))
  ([^double r ^double g ^double b ^long samples-per-pixel] (->color (Vec3. r g b) samples-per-pixel))
  ([in ^long samples-per-pixel] (-> in
                                    (v/div samples-per-pixel)
                                    (v/fmap linear-to-gamma)
                                    (c/scale-up))))

(defn <-color
  "Convert clojure2d color to a vec3"
  [c]
  (let [^Vec4 c (c/scale-down c)]
    (Vec3. (.x c) (.y c) (.z c))))
