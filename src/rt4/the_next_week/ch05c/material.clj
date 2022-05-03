(ns rt4.the-next-week.ch05c.material
  (:require [rt4.common :as common]
            [rt4.the-next-week.ch05c.ray :as ray]
            [rt4.the-next-week.ch05c.texture :as texture]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol MaterialProto
  (scatter [material ray-in rec]))

(defrecord MaterialData [attenuation scattered])

(defrecord Lambertian [albedo]
  MaterialProto
  (scatter [_ ray-in rec]
    (let [scatter-direction (v/add (:normal rec) (common/random-unit-vector))
          attenuation (texture/value albedo (:u rec) (:v rec) (:p rec))]
      (if (v/is-near-zero? scatter-direction)
        (->MaterialData attenuation (ray/ray (:p rec) (:normal rec) (:time ray-in)))
        (->MaterialData attenuation (ray/ray (:p rec) scatter-direction (:time ray-in)))))))

(defn lambertian [albedo]
  (if (instance? Vec3 albedo)
    (->Lambertian (texture/solid-color albedo))
    (->Lambertian albedo)))

(defrecord Metal [albedo ^double fuzz]
  MaterialProto
  (scatter [_ ray-in rec]
    (let [reflected (v/add (common/reflect (v/normalize (:direction ray-in)) (:normal rec))
                           (v/mult (common/random-in-unit-sphere) fuzz))]
      (when (pos? (v/dot reflected (:normal rec)))
        (->MaterialData albedo (ray/ray (:p rec) reflected (:time ray-in)))))))

(defn metal [albedo ^double fuzz]
  (->Metal albedo (min fuzz 1.0)))

(def one (v/vec3 1.0 1.0 1.0))

(defn- reflectance
  ^double [^double cosine ^double ref-idx]
  (let [r0 (m/sq (/ (- 1.0 ref-idx) (inc ref-idx)))]
    (+ r0 (* (- 1.0 r0) (m/pow (- 1.0 cosine) 5.0)))))

(defrecord Dielectric [^double ir]
  MaterialProto
  (scatter [_ ray-in rec]
    (let [refraction-ratio (if (:front-face? rec) (/ ir) ir)
          unit-direction (v/normalize (:direction ray-in))
          cos-theta (min (v/dot (v/sub unit-direction) (:normal rec)) 1.0)
          sin-theta (m/sqrt (- 1.0 (* cos-theta cos-theta)))
          cannot-refract? (pos? (dec (* refraction-ratio sin-theta)))
          direction (if (or cannot-refract?
                            (> (reflectance cos-theta refraction-ratio) (r/drand)))
                      (common/reflect unit-direction (:normal rec))
                      (common/refract unit-direction (:normal rec) refraction-ratio))]
      (->MaterialData one (ray/ray (:p rec) direction (:time ray-in))))))

(defn dielectric [ir]
  (->Dielectric ir))
