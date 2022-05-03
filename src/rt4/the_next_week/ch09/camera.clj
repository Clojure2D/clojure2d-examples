(ns rt4.the-next-week.ch09.camera
  (:require  [rt4.the-next-week.ch09.ray :as ray]
             [fastmath.vector :as v]
             [fastmath.core :as m]
             [rt4.common :as common]
             [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol CameraProto
  (get-ray [camera s t]))

(defrecord Camera [origin lower-left-corner horizontal vertical
                   u v w ^double lens-radius]
  CameraProto
  (get-ray [_ s t]
    (let [^Vec2 rd (v/mult (common/random-in-unit-disc) lens-radius)
          offset (v/add (v/mult u (.x rd))
                        (v/mult v (.y rd)))
          ray-time (r/drand)]
      (ray/ray (v/add origin offset)
               (-> lower-left-corner
                   (v/add (v/mult horizontal s))
                   (v/add (v/mult vertical t))
                   (v/sub origin)
                   (v/sub offset))
               ray-time))))

(def default-config {:vfov 40.0 :aspect-ratio (/ 16.0 9.0)
                   :lookfrom (v/vec3 0.0 0.0 -1.0)
                   :lookat (v/vec3 0.0 0.0 0.0)
                   :vup (v/vec3 0.0 1.0 0.0)
                   :aperture 0.0
                   :focus-dist 10.0})

(defn camera
  ([] (camera {}))
  ([config]
   (let [{:keys [^double vfov ^double aspect-ratio
                 ^double aperture ^double focus-dist
                 lookfrom lookat vup]} (merge default-config config)
         theta (m/radians vfov)
         h (m/tan (/ theta 2.0))
         viewport-height (* 2.0 h)
         viewport-width (* aspect-ratio viewport-height)

         w (v/normalize (v/sub lookfrom lookat))
         u (v/normalize (v/cross vup w))
         v (v/cross w u)
         
         origin lookfrom
         horizontal (v/mult u (* focus-dist viewport-width))
         vertical (v/mult v (* focus-dist viewport-height))
         lower-left-corner (-> origin
                               (v/sub (v/div horizontal 2.0))
                               (v/sub (v/div vertical 2.0))
                               (v/sub (v/mult w focus-dist)))
         lens-radius (/ aperture 2.0)]
     (->Camera origin lower-left-corner horizontal vertical u v w lens-radius))))
