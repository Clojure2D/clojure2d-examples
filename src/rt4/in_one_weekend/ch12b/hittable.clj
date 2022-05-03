(ns rt4.in-one-weekend.ch12b.hittable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.in-one-weekend.ch12b.interval :as interval]))

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

(defn hit-list
  "Traverse all scene objects."
  [xs ray ray-t]
  (reduce (fn [curr-hit object]
            (if-let [hit-object (hit object ray (interval/interval (:mn ray-t)
                                                                   (or (:t curr-hit) (:mx ray-t))))]
              hit-object
              curr-hit)) nil xs))
