(ns rt4.in-one-weekend.ch12a.camera
  (:require  [rt4.in-one-weekend.ch12a.ray :as ray]
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

(def default-config {:vfov 40.0 :aspect-ratio (/ 16.0 9.0)})

(defn camera
  ([] (camera {}))
  ([config]
   (let [{:keys [^double vfov ^double aspect-ratio]} (merge default-config config)
         theta (m/radians vfov)
         h (m/tan (/ theta 2.0))
         viewport-height (* 2.0 h)
         viewport-width (* aspect-ratio viewport-height)
         focal-length 1.0
         origin (v/vec3 0.0 0.0 0.0)
         horizontal (v/vec3 viewport-width 0.0 0.0)
         vertical (v/vec3 0.0 viewport-height 0.0)
         lower-left-corner (-> origin
                               (v/sub (v/div horizontal 2.0))
                               (v/sub (v/div vertical 2.0))
                               (v/sub (v/vec3 0.0 0.0 focal-length)))]
     (->Camera origin lower-left-corner horizontal vertical))))
