(ns rt4.the-next-week.ch04a.ray
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol RayProto
  (at [ray t]))

(defrecord Ray [origin direction ^double time]
  RayProto
  (at [_ t] (v/add origin (v/mult direction t))))

(defn ray
  ([m] (map->Ray (merge {:time 0.0} m)))
  ([origin direction] (->Ray origin direction 0.0))
  ([origin direction time] (->Ray origin direction time)))
