(ns RTinWeekend.camera
  (:require [RTinWeekend.ray :refer :all]
            [fastmath.vector :as v]))

(defrecord Camera [origin lower-left-corner horizontal vertical])

(def ^:const lower-left-corner (v/vec3 -2.0 -1.0 -1.0))
(def ^:const horizontal (v/vec3 4.0 0.0 0.0))
(def ^:const vertical (v/vec3 0.0 2.0 0.0))
(def ^:const origin (v/vec3 0.0 0.0 0.0))

(def default-camera
  (->Camera origin lower-left-corner horizontal vertical))

(defn get-ray
  ""
  [camera u v]
  (->Ray (:origin camera) (v/add (v/add (v/mult (:horizontal camera) u) (v/mult (:vertical camera) v))
                                 (:lower-left-corner camera))))
