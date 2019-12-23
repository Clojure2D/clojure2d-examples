(ns rt-in-weekend.sphere
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m])
  (:import [rt_in_weekend.ray Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Sphere [center ^double radius material]
  HitableProto
  (hit [_ r t-min t-max]
    (let [oc (v/sub (.origin ^Ray r) center)
          a (v/magsq (.direction ^Ray r))
          b- (- (v/dot oc (.direction ^Ray r)))
          c (- (v/dot oc oc) (* radius radius))
          discriminant (- (* b- b-) (* a c))]
      (when (pos? discriminant)
        (let [dsqrt (m/sqrt discriminant)
              temp (/ (- b- dsqrt) a)]
          (if (and (< temp ^double t-max)
                   (> temp ^double t-min))
            (let [patp (point-at-parameter r temp)]
              (->HitData temp patp (v/div (v/sub patp center) radius) material))
            (let [temp2 (/ (+ b- dsqrt) a)]
              (when (and (< temp ^double t-max)
                         (> temp ^double t-min))
                (let [patp (point-at-parameter r temp)]
                  (->HitData temp patp (v/div (v/sub patp center) radius) material))))))))))

