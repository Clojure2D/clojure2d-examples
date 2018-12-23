(ns rt-in-weekend.ray
  (:require [fastmath.vector :as v]))

(defprotocol RayProto
  (point-at-parameter [r t]))

(deftype Ray [origin direction]
  RayProto
  (point-at-parameter [_ t]
    (v/add origin (v/mult direction t))))
