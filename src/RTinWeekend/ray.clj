(ns RTinWeekend.ray
  (:require [fastmath.vector :as v]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3]))

(defrecord Ray [^Vec3 origin ^Vec3 direction])

(defn point-at-parameter
  ""
  [ray t]
  (v/add (:origin ray) (v/mult (:direction ray) t)))
