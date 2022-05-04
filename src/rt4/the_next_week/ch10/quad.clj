(ns rt4.the-next-week.ch10.quad
  (:require [fastmath.core :as m]
            [rt4.the-next-week.ch10.hittable :as hittable]
            [rt4.the-next-week.ch10.aabb :as aabb]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch10.interval :as interval]
            [rt4.the-next-week.ch10.ray :as ray]
            [rt4.the-next-week.ch10.hittable-list :as hittable-list])
  (:import [fastmath.vector Vec3]
           [rt4.the_next_week.ch10.ray Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Quad [Q u v mat bbox normal ^double D w]
  hittable/HittableProto
  (hit [_ r ray-t]
    (let [denom (v/dot normal (.direction ^Ray r))]
      (when (>= (m/abs denom) 1.0e-8)
        (let [t (/ (- D (v/dot normal (.origin ^Ray r))) denom)]
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

(defn box
  [^Vec3 a ^Vec3 b mat]
  (let [^Vec3 mn (v/vec3 (min (.x a) (.x b)) (min (.y a) (.y b)) (min (.z a) (.z b)))
        ^Vec3 mx (v/vec3 (max (.x a) (.x b)) (max (.y a) (.y b)) (max (.z a) (.z b)))
        ^Vec3 d (v/sub mx mn)
        dx (v/vec3 (.x d) 0.0 0.0)
        dy (v/vec3 0.0 (.y d) 0.0)
        dz (v/vec3 0.0 0.0 (.z d))]
    (hittable-list/hittable-list (quad (v/vec3 (.x mn) (.y mn) (.z mx)) dx dy mat)
                                 (quad (v/vec3 (.x mx) (.y mn) (.z mx)) (v/sub dz) dy mat)
                                 (quad (v/vec3 (.x mx) (.y mn) (.z mn)) (v/sub dx) dy mat)
                                 (quad (v/vec3 (.x mn) (.y mn) (.z mn)) dz dy mat)
                                 (quad (v/vec3 (.x mn) (.y mx) (.z mx)) dx (v/sub dz) mat)
                                 (quad (v/vec3 (.x mn) (.y mn) (.z mn)) dx dz mat))))
