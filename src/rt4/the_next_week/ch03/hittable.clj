(ns rt4.the-next-week.ch03.hittable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol HittableProto
  (hit [object r ray-t]))

(defrecord HitData [p normal mat ^double t front-face?])

(defn hit-data
  ([r {:keys [p normal mat t]}]
   (hit-data r p normal mat t))
  ([r p normal mat t]
   (let [front-face? (neg? (v/dot (:direction r) normal))]
     (->HitData p (if front-face? normal (v/sub normal)) mat t front-face?))))
