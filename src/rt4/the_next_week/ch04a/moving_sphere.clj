(ns rt4.the-next-week.ch04a.moving-sphere
  (:require [rt4.the-next-week.ch04a.hittable :as hittable]
            [rt4.the-next-week.ch04a.interval :as interval]
            [rt4.the-next-week.ch04a.ray :as ray]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch04a.aabb :as aabb]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- center
  [center0 center-vec ^double time]
  (v/add center0 (v/mult center-vec time)))

(defrecord Sphere [center0 center-vec ^double radius mat bbox]
  hittable/HittableProto
  (hit [_ r ray-t]
    (let [sphere-center (center center0 center-vec (:time r))
          oc (v/sub (:origin r) sphere-center)
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
              (hittable/hit-data r p (v/div (v/sub p sphere-center) radius) mat root))))))))

(defn sphere
  ([{:keys [center0 center1 ^double radius mat]}] (sphere center0 center1 radius mat))
  ([center0 center1 ^double radius mat]
   (let [rvec (v/vec3 radius radius radius)
         box0 (aabb/aabb (v/sub center0 rvec) (v/add center0 rvec))
         box1 (aabb/aabb (v/sub center1 rvec) (v/add center1 rvec))]
     (->Sphere center0 (v/sub center1 center0) radius mat (aabb/merge-boxes box0 box1)))))
