(ns rt4.in-one-weekend.ch06b.hittable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol HittableProto
  (hit [object r ray-tmin ray-tmax]))

(defrecord HitData [p normal ^double t front-face?])

(defn hit-data
  ([r {:keys [p normal t]}]
   (hit-data r p normal t))
  ([r p normal t]
   (let [front-face? (neg? (v/dot (:direction r) normal))]
     (->HitData p (if front-face? normal (v/sub normal)) t front-face?))))

(defn hit-list
  "Traverse all scene objects."
  [xs ray ray-tmin ray-tmax]
  (reduce (fn [curr-hit object]
            (if-let [hit-object (hit object ray ray-tmin (or (:t curr-hit) ray-tmax))]
              hit-object
              curr-hit)) nil xs))
