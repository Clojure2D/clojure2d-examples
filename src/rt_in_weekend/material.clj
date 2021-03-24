(ns rt-in-weekend.material
  (:require [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all])
  (:import [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol MaterialProto
  (scatter [m ray-in hit-data]))

#_(defn- random-in-unit-sphere []
    (let [v (v/vec3 (r/drand -1.0 1.0) (r/drand -1.0 1.0) (r/drand -1.0 1.0))]
      (if (< ^double (v/magsq v) 1.0) v (recur))))

(defn- random-in-unit-sphere []
  (let [r (m/cbrt (r/drand))
        u (r/drand -1.0 1.0)
        ur (* r (m/sqrt (- 1.0 (* u u))))
        phi (r/drand m/TWO_PI)]
    (v/vec3  (* ur (m/cos phi))
             (* ur (m/sin phi))
             (* r u))))

(deftype Lambertian [albedo]
  MaterialProto
  (scatter [_ ray-in hit-data]
    (let [target (v/add (.p ^HitData hit-data) (v/add (.normal ^HitData hit-data) (random-in-unit-sphere)))]
      [albedo (->Ray (.p ^HitData hit-data) (v/sub target (.p ^HitData hit-data)))])))

(defn- reflect [v n]
  (v/sub v (v/mult n (* 2.0 ^double (v/dot v n)))))

(deftype Metal [albedo ^double fuzz]
  MaterialProto
  (scatter [_ ray-in hit-data]
    (let [reflected (reflect (v/normalize (.direction ^Ray ray-in)) (.normal ^HitData hit-data))
          scattered (->Ray (.p ^HitData hit-data) (if-not (pos? fuzz)
                                                    reflected
                                                    (v/add reflected (v/mult (random-in-unit-sphere) fuzz))))]
      (when (pos? (v/dot (.direction ^Ray scattered) (.normal ^HitData hit-data)))
        [albedo scattered]))))

(defn- refract [v n ^double ni-over-nt]
  (let [uv (v/normalize v)
        dt (v/dot uv n)
        discriminant (- 1.0 (* ni-over-nt ni-over-nt (- 1.0 (* dt dt))))]
    (when (pos? discriminant)
      (v/sub (v/mult (v/sub uv (v/mult n dt)) ni-over-nt)
             (v/mult n (m/sqrt discriminant))))))

(defn- schlick ^double [^double cosine ^double ref-idx]
  (let [r0 (m/sq (/ (- 1.0 ref-idx) (inc ref-idx)))]
    (+ r0 (* (- 1.0 r0) (m/pow (- 1.0 cosine) 5.0)))))

(def ^:private ^:const one (v/vec3 1.0 1.0 1.0))

(defrecord Dielectric [^double ref-idx]
  MaterialProto
  (scatter [_ ray-in hit-data]
    (let [dot (v/dot (.direction ^Ray ray-in) (.normal ^HitData hit-data))
          [outward-normal ni-over-nt cosine] (if (pos? dot)
                                               [(v/sub (.normal ^HitData hit-data))
                                                ref-idx
                                                (/ (* ref-idx dot) (v/mag (.direction ^Ray ray-in)))]
                                               [(.normal ^HitData hit-data)
                                                (/ ref-idx)
                                                (- (/ dot (v/mag (.direction ^Ray ray-in))))])
          refracted (refract (.direction ^Ray ray-in) outward-normal ni-over-nt)]
      [one (if (and refracted (> (r/drand) (schlick cosine ref-idx)))
             (->Ray (.p ^HitData hit-data) refracted)
             (->Ray (.p ^HitData hit-data) (reflect (.direction ^Ray ray-in) (.normal ^HitData hit-data))))])))
