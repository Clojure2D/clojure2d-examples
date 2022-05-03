(ns rt4.in-one-weekend.ch04.ray
  (:require [fastmath.vector :as v]))

(defprotocol RayProto
  (at [ray t]))

(defrecord Ray [origin direction]
  RayProto
  (at [_ t] (v/add origin (v/mult direction t))))

(defn ray
  [origin direction]
  (->Ray origin direction))
