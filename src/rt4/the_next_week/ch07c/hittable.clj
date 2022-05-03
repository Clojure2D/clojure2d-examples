(ns rt4.the-next-week.ch07c.hittable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol HittableProto
  (hit [object r ray-t]))

(defrecord HitData [p normal mat ^double t u v front-face?])

(defn hit-data
  ([r {:keys [p normal mat t u v]
       :or {u 0.0 v 0.0}}]
   (hit-data r p normal mat t u v))
  ([r p normal mat t]
   (hit-data r p normal mat t 0.0 0.0))
  ([r p normal mat t u v]
   (let [front-face? (neg? (v/dot (:direction r) normal))]
     (->HitData p (if front-face? normal (v/sub normal)) mat t u v front-face?))))
