(ns RTinWeekend.sphere
  (:require [RTinWeekend.ray :refer :all]
            [RTinWeekend.hitable :refer :all]
            [fastmath.vector :as v]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Sphere [^Vec3 center ^double radius]
  Hitable
  (hit [_ r t-min t-max]
    (let [oc (v/sub (:origin r) center)
          ^double a (v/dot (:direction r) (:direction r))
          b- (- ^double (v/dot oc (:direction r)))
          c (- ^double (v/dot oc oc) (* radius radius))
          discriminant (- (* b- b-) (* a c))]
      (when (pos? discriminant)
        (let [dsqrt (m/sqrt discriminant)
              temp (/ (- b- dsqrt) a)]
          (if (and (< temp ^double t-max)
                   (> temp ^double t-min))
            (let [patp (point-at-parameter r temp)]
              (->hit-record temp patp (v/div (v/sub patp center) radius)))
            (let [temp2 (/ (+ b- dsqrt) a)]
              (when (and (< temp ^double t-max)
                         (> temp ^double t-min))
                (let [patp (point-at-parameter r temp)]
                  (->hit-record temp patp (v/div (v/sub patp center) radius)))))))))))
