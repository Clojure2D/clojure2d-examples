(ns rt4.in-one-weekend.ch07.ray
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol RayProto
  (at [ray t]))

(defrecord Ray [origin direction]
  RayProto
  (at [_ t] (v/add origin (v/mult direction t))))

(defn ray
  [origin direction]
  (->Ray origin direction))
