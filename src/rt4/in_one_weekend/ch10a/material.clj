(ns rt4.in-one-weekend.ch10a.material
  (:require [rt4.common :as common]
            [rt4.in-one-weekend.ch10a.ray :as ray]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol MaterialProto
  (scatter [material ray-in rec]))

(defrecord MaterialData [attenuation scattered])

(defrecord Lambertian [albedo]
  MaterialProto
  (scatter [_ _ray-in rec]
    (let [scatter-direction (v/add (:normal rec) (common/random-unit-vector))]
      (if (v/is-near-zero? scatter-direction)
        (->MaterialData albedo (ray/ray (:p rec) (:normal rec)))
        (->MaterialData albedo (ray/ray (:p rec) scatter-direction))))))

(defn lambertian [albedo] (->Lambertian albedo))

(defrecord Metal [albedo ^double fuzz]
  MaterialProto
  (scatter [_ ray-in rec]
    (let [reflected (v/add (common/reflect (v/normalize (:direction ray-in)) (:normal rec))
                           (v/mult (common/random-in-unit-sphere) fuzz))]
      (when (pos? (v/dot reflected (:normal rec)))
        (->MaterialData albedo (ray/ray (:p rec) reflected))))))

(defn metal [albedo ^double fuzz]
  (->Metal albedo (min fuzz 1.0)))

(def one (v/vec3 1.0 1.0 1.0))

(defrecord Dielectric [^double ir]
  MaterialProto
  (scatter [_ ray-in rec]
    (let [refraction-ratio (if (:front-face? rec) (/ ir) ir)
          unit-direction (v/normalize (:direction ray-in))
          refracted (common/refract unit-direction (:normal rec) refraction-ratio)]
      (->MaterialData one (ray/ray (:p rec) refracted)))))

(defn dielectric [ir]
  (->Dielectric ir))
