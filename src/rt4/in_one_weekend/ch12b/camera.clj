(ns rt4.in-one-weekend.ch12b.camera
  (:require  [rt4.in-one-weekend.ch12b.ray :as ray]
             [fastmath.vector :as v]
             [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol CameraProto
  (get-ray [camera s t]))

(defrecord Camera [origin lower-left-corner horizontal vertical]
  CameraProto
  (get-ray [_ s t]
    (ray/ray origin (-> lower-left-corner
                        (v/add (v/mult horizontal s))
                        (v/add (v/mult vertical t))
                        (v/sub origin)))))

(def default-config {:vfov 40.0 :aspect-ratio (/ 16.0 9.0)
                   :lookfrom (v/vec3 0.0 0.0 -1.0)
                   :lookat (v/vec3 0.0 0.0 0.0)
                   :vup (v/vec3 0.0 1.0 0.0)})

(defn camera
  ([] (camera {}))
  ([config]
   (let [{:keys [^double vfov ^double aspect-ratio
                 lookfrom lookat vup]} (merge default-config config)
         theta (m/radians vfov)
         h (m/tan (/ theta 2.0))
         viewport-height (* 2.0 h)
         viewport-width (* aspect-ratio viewport-height)

         w (v/normalize (v/sub lookfrom lookat))
         u (v/normalize (v/cross vup w))
         v (v/cross w u)
         
         origin lookfrom
         horizontal (v/mult u viewport-width)
         vertical (v/mult v viewport-height)
         lower-left-corner (-> origin
                               (v/sub (v/div horizontal 2.0))
                               (v/sub (v/div vertical 2.0))
                               (v/sub w))]
     (->Camera origin lower-left-corner horizontal vertical))))
