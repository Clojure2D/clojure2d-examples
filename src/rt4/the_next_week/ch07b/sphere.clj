(ns rt4.the-next-week.ch07b.sphere
  (:require [rt4.the-next-week.ch07b.hittable :as hittable]
            [rt4.the-next-week.ch07b.interval :as interval]
            [rt4.the-next-week.ch07b.ray :as ray]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch07b.aabb :as aabb])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- get-sphere-uv
  ^Vec2 [^Vec3 p]
  (let [theta (m/acos (- (.y p)))
        phi (+ (m/atan2 (- (.z p)) (.x p)) m/PI)]
    (Vec2. (/ phi m/TWO_PI) (/ theta m/PI))))

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
            (let [p (ray/at r root)
                  outward-normal (v/div (v/sub p center) radius)
                  uv (get-sphere-uv outward-normal)]
              (hittable/hit-data r p outward-normal mat root (.x uv) (.y uv)))))))))

(defn sphere
  ([{:keys [center ^double radius mat]}] (sphere center radius mat))
  ([center ^double radius mat]
   (let [rvec (v/vec3 radius radius radius)]
     (->Sphere center radius mat (aabb/aabb (v/sub center rvec) (v/add center rvec))))))
