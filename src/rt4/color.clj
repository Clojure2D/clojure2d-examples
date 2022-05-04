(ns rt4.color
  (:require [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn ->color
  "Convert to clojure2d color, optionally divide by a given constant"
  (^Vec4 [^double r ^double g ^double b] (->color (Vec3. r g b)))
  ([in] (-> in
            (v/sqrt) ;; linear to gamma
            (c/scale-up)))
  ([^double r ^double g ^double b ^long samples-per-pixel] (->color (Vec3. r g b) samples-per-pixel))
  ([in ^long samples-per-pixel] (-> in
                                    (v/div samples-per-pixel)
                                    (v/sqrt) ;; linear-to-gamma
                                    (c/scale-up))))

(defn <-color
  "Convert clojure2d color to a vec3"
  ^Vec3 [c]
  (let [^Vec4 c (c/scale-down c)]
    (Vec3. (.x c) (.y c) (.z c))))
