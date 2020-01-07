(ns rt-in-weekend.camera
  (:require [rt-in-weekend.ray :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.protocols :as p]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol CameraProto
  (get-ray [_ u v]))

(deftype FirstCamera [origin lower-left-corner horizontal vertical]
  CameraProto
  (get-ray [_ u v] (->Ray origin (p/add (p/add (p/mult horizontal u) (p/mult vertical v))
                                        lower-left-corner))))

(def ^:const lower-left-corner (v/vec3 -2.0 -1.0 -1.0))
(def ^:const horizontal (v/vec3 4.0 0.0 0.0))
(def ^:const vertical (v/vec3 0.0 2.0 0.0))
(def ^:const origin (v/vec3 0.0 0.0 0.0))

(def default-camera
  (->FirstCamera origin lower-left-corner horizontal vertical))

(defn- random-in-unit-disc  []
  (let [v (v/vec2 (r/drand -1.0 1.0) (r/drand -1.0 1.0))]
    (if (< ^double (v/magsq v) 1.0) v (recur))))

(defn positionable-camera
  ([^double vfov ^double aspect]
   (let [theta (* vfov (/ m/PI 180.0))
         half-height (m/tan (* 0.5 theta))
         half-width (* aspect half-height)
         lower-left-corner (v/vec3 (- half-width) (- half-height) -1.0)
         horizontal (v/vec3 (+ half-width half-width) 0.0 0.0)
         vertical (v/vec3 0.0 (+ half-height half-height) 0.0)
         origin (v/vec3 0.0 0.0 0.0)]
     (reify CameraProto
       (get-ray [_ u v] (->Ray origin (v/sub (v/add (v/add (v/mult horizontal u) (v/mult vertical v))
                                                    lower-left-corner)
                                             origin))))))
  ([lookfrom lookat vup vfov aspect]
   (let [theta (* ^double vfov (/ m/PI 180.0))
         half-height (m/tan (* 0.5 theta))
         half-width (* ^double aspect half-height)
         origin lookfrom
         w (v/normalize (v/sub lookfrom lookat))
         u (v/normalize (v/cross vup w))
         v (v/cross w u)
         lower-left-corner (v/sub (v/sub (v/sub origin (v/mult u half-width)) (v/mult v half-height)) w)
         horizontal (v/mult u (* 2.0 half-width))
         vertical (v/mult v (* 2.0 half-height))]
     (reify CameraProto
       (get-ray [_ u v] (->Ray origin (v/sub (v/add (v/add (v/mult horizontal u) (v/mult vertical v))
                                                    lower-left-corner)
                                             origin))))))
  ([lookfrom lookat vup vfov aspect aperture focus-dist]
   (let [lens-radius (* 0.5 ^double aperture)
         theta (* ^double vfov (/ m/PI 180.0))
         half-height (* ^double focus-dist (m/tan (* 0.5 theta)))
         half-width (* ^double aspect half-height)
         origin lookfrom
         w (v/normalize (v/sub lookfrom lookat))
         u (v/normalize (v/cross vup w))
         v (v/cross w u)
         lower-left-corner (v/sub (v/sub (v/sub origin (v/mult u half-width))
                                         (v/mult v half-height))
                                  (v/mult w focus-dist))
         horizontal (v/mult u (* 2.0 half-width))
         vertical (v/mult v (* 2.0 half-height))]
     (reify CameraProto
       (get-ray [_ s t]
         (let [^Vec2 rd (v/mult (random-in-unit-disc) lens-radius)
               offset (v/add (v/mult u (.x rd))
                             (v/mult v (.y rd)))]
           (->Ray (v/add origin offset) (v/sub (v/sub (v/add (v/add (v/mult horizontal s) (v/mult vertical t))
                                                             lower-left-corner)
                                                      origin) offset))))))))
