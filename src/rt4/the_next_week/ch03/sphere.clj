(ns rt4.the-next-week.ch03.sphere
  (:require [rt4.the-next-week.ch03.hittable :as hittable]
            [rt4.the-next-week.ch03.interval :as interval]
            [rt4.the-next-week.ch03.ray :as ray]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch03.aabb :as aabb]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Sphere [center ^double radius mat bbox]
  hittable/HittableProto
  (hit [_ r ray-t]
    (let [oc (v/sub (:origin r) center)
          a (v/magsq (:direction r))
          half-b (v/dot oc (:direction r))
          c (- (v/magsq oc) (* radius radius))
          discriminant (- (* half-b half-b) (* a c))]
      (when-not (neg? discriminant)
        (let [sqrtd (m/sqrt discriminant)
              root (let [root (/ (- (- half-b) sqrtd) a)]
                     (if-not (interval/contains- ray-t root)
                       (let [root (/ (+ (- half-b) sqrtd) a)]
                         (when (interval/contains- ray-t root) 
                           root))
                       root))]
          (when root
            (let [p (ray/at r root)]
              (hittable/hit-data r p (v/div (v/sub p center) radius) mat root))))))))

(defn sphere
  ([{:keys [center ^double radius mat]}] (sphere center radius mat))
  ([center ^double radius mat]
   (let [rvec (v/vec3 radius radius radius)]
     (->Sphere center radius mat (aabb/aabb (v/sub center rvec) (v/add center rvec))))))
