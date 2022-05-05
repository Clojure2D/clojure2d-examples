(ns rt4.the-rest-of-your-life.ch06a.material
  (:require [rt4.common :as common]
            [rt4.the-rest-of-your-life.ch06a.ray :as ray]
            [rt4.the-rest-of-your-life.ch06a.texture :as texture]
            [rt4.the-rest-of-your-life.ch06a.hittable]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [rt4.the_rest_of_your_life.ch06a.hittable HitData]
           [rt4.the_rest_of_your_life.ch06a.ray Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol MaterialProto
  (scatter [material ray-in rec])
  (scattering-pdf [material ray-in rec scattered])
  (emitted [material u v p]))

(defrecord MaterialData [attenuation scattered])

(def ^:private black (v/vec3 0.0 0.0 0.0))

(defrecord Lambertian [albedo]
  MaterialProto
  (emitted [_ _ _ _] black)
  (scattering-pdf [_ _ray-in rec scattered]
    (let [cos-theta (v/dot (.normal ^HitData rec) (v/normalize (.direction ^Ray scattered)))]
      (max 0.0 (/ cos-theta m/PI))))
  (scatter [_ ray-in rec]
    (let [scatter-direction (v/add (.normal ^HitData rec) (common/random-unit-vector))
          attenuation (texture/value albedo (.u ^HitData rec) (.v ^HitData rec) (.p ^HitData rec))]
      (if (v/is-near-zero? scatter-direction)
        (->MaterialData attenuation (ray/ray (.p ^HitData rec) (.normal ^HitData rec) (.time ^Ray ray-in)))
        (->MaterialData attenuation (ray/ray (.p ^HitData rec) scatter-direction (.time ^Ray ray-in)))))))

(defn lambertian [albedo]
  (->Lambertian albedo))

(defrecord Metal [albedo ^double fuzz]
  MaterialProto
  (emitted [_ _ _ _] black)
  (scatter [_ ray-in rec]
    (let [reflected (v/add (common/reflect (v/normalize (.direction ^Ray ray-in)) (.normal ^HitData rec))
                           (v/mult (common/random-in-unit-sphere) fuzz))]
      (when (pos? (v/dot reflected (.normal ^HitData rec)))
        (->MaterialData albedo (ray/ray (.p ^HitData rec) reflected (.time ^Ray ray-in)))))))

(defn metal [albedo ^double fuzz]
  (->Metal albedo (min fuzz 1.0)))

(def one (v/vec3 1.0 1.0 1.0))

(defn- reflectance
  ^double [^double cosine ^double ref-idx]
  (let [r0 (m/sq (/ (- 1.0 ref-idx) (inc ref-idx)))]
    (+ r0 (* (- 1.0 r0) (m/pow (- 1.0 cosine) 5.0)))))

(defrecord Dielectric [^double ir]
  MaterialProto
  (emitted [_ _ _ _] black)
  (scatter [_ ray-in rec]
    (let [refraction-ratio (if (.front-face? ^HitData rec) (/ ir) ir)
          unit-direction (v/normalize (.direction ^Ray ray-in))
          cos-theta (min (v/dot (v/sub unit-direction) (.normal ^HitData rec)) 1.0)
          sin-theta (m/sqrt (- 1.0 (* cos-theta cos-theta)))
          cannot-refract? (pos? (dec (* refraction-ratio sin-theta)))
          direction (if (or cannot-refract?
                            (> (reflectance cos-theta refraction-ratio) (r/drand)))
                      (common/reflect unit-direction (.normal ^HitData rec))
                      (common/refract unit-direction (.normal ^HitData rec) refraction-ratio))]
      (->MaterialData one (ray/ray (.p ^HitData rec) direction (.time ^Ray ray-in))))))

(defn dielectric [ir]
  (->Dielectric ir))

;;

(defrecord DiffuseLight [emit]
  MaterialProto
  (scatter [_ _ _] nil)
  (emitted [_ u v p] (texture/value emit u v p)))

(defn diffuse-light
  [texture-or-color]
  (->DiffuseLight texture-or-color))

;;

(defrecord Isotropic [albedo]
  MaterialProto
  (emitted [_ _ _ _] black)
  (scatter [_ ray-in rec]
    (->MaterialData (texture/value albedo (.u ^HitData rec) (.v ^HitData rec) (.p ^HitData rec))
                    (ray/ray (.p ^HitData rec) (common/random-unit-vector) (.time ^Ray ray-in)))))

(defn isotropic [albedo]
  (->Isotropic albedo))
