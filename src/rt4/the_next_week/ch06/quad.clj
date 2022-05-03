(ns rt4.the-next-week.ch06.quad
  (:require [fastmath.core :as m]
            [rt4.the-next-week.ch06.hittable :as hittable]
            [rt4.the-next-week.ch06.aabb :as aabb]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch06.interval :as interval]
            [rt4.the-next-week.ch06.ray :as ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Quad [Q u v mat bbox normal ^double D w]
  hittable/HittableProto
  (hit [_ r ray-t]
    (let [denom (v/dot normal (:direction r))]
      (when (>= (m/abs denom) 1.0e-8)
        (let [t (/ (- D (v/dot normal (:origin r))) denom)]
          (when (interval/contains- ray-t t)
            (let [intersection (ray/at r t)
                  planar-hitp-vector (v/sub intersection Q)
                  alpha (v/dot w (v/cross planar-hitp-vector v))
                  beta (v/dot w (v/cross u planar-hitp-vector))]
              (when-not (or (neg? alpha) (< 1.0 alpha)
                            (neg? beta)  (< 1.0 beta))
                (hittable/hit-data r intersection normal mat t alpha beta)))))))))

(defn quad
  [Q u v mat]
  (let [box (aabb/pad (aabb/aabb Q (v/add (v/add Q u) v)))
        n (v/cross u v)
        normal (v/normalize n)]
    (->Quad Q u v mat box normal (v/dot normal Q) (v/div n (v/dot n n)))))
