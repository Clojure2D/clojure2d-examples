(ns rt4.in-one-weekend.ch06b.sphere
  (:require [rt4.in-one-weekend.ch06b.hittable :as hittable]
            [rt4.in-one-weekend.ch06b.ray :as ray]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- not-contains?
  [^double ray-tmin ^double ray-tmax ^double root]
  (or (< root ray-tmin)
      (< ray-tmax root)))

(defrecord Sphere [center ^double radius]
  hittable/HittableProto
  (hit [_ r ray-tmin ray-tmax]
    (let [oc (v/sub (:origin r) center)
          a (v/magsq (:direction r))
          half-b (v/dot oc (:direction r))
          c (- (v/magsq oc) (* radius radius))
          discriminant (- (* half-b half-b) (* a c))]
      (when-not (neg? discriminant)
        (let [sqrtd (m/sqrt discriminant)
              root (let [root (/ (- (- half-b) sqrtd) a)]
                     (if (not-contains? ray-tmin ray-tmax root)
                       (let [root (/ (+ (- half-b) sqrtd) a)]
                         (when-not (not-contains? ray-tmin ray-tmax root) 
                           root))
                       root))]
          (when root
            (let [p (ray/at r root)]
              (hittable/hit-data r p (v/div (v/sub p center) radius) root))))))))

(defn sphere
  ([map] (map->Sphere map))
  ([center radius]
   (->Sphere center radius)))
