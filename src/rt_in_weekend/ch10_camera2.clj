(ns rt-in-weekend.ch10-camera2
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.camera :refer :all]
            [rt-in-weekend.material :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(def ^:const zero (v/vec3 0.0 0.0 0.0))

(def world [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5 (->Lambertian (v/vec3 0.1 0.2 0.5)))
            (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 (->Lambertian (v/vec3 0.8 0.8 0.0)))
            (->Sphere (v/vec3 1.0 0.0 -1.0) 0.5 (->Metal (v/vec3 0.8 0.6 0.2) 0.0))
            (->Sphere (v/vec3 -1.0 0.0 -1.0) 0.5 (->Dielectric 1.5))
            (->Sphere (v/vec3 -1.0 0.0 -1.0) -0.45 (->Dielectric 1.5))])

(defn color
  ([ray world] (color ray world 50))
  ([^Ray ray world ^long depth]
   (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
     (let [[attenuation scattered] (scatter (.material world-hit) ray world-hit)]
       (if (and attenuation (pos? depth))
         (v/emult attenuation (color scattered world (dec depth)))
         zero))
     (let [^Vec3 unit (v/normalize (.direction ray))
           t (* 0.5 (inc (.y unit)))]
       (v/interpolate v1 v2 t)))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 200)

(def img (p/pixels nx ny))

(def camera (positionable-camera (v/vec3 -2 2 1) (v/vec3 0 0 -1) (v/vec3 0 1 0) 90 (/ (double nx) ny)))
;; (def camera (positionable-camera (v/vec3 -2 2 1) (v/vec3 0 0 -1) (v/vec3 0 1 0) 20 (/ (double nx) ny)))

(time (dotimes [j ny]
        (println (str "Line: " j))
        (dotimes [i nx]
          (let [col (reduce v/add zero
                            (repeatedly samples #(let [u (/ (+ (r/drand) i) nx)
                                                       v (/ (+ (r/drand) j) ny)
                                                       r (get-ray camera u v)]
                                                   (color r world))))]
            (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                   (v/sqrt)
                                                   (v/mult 255.0)))))))

(u/show-image img)

;; (save img "results/rt-in-weekend/camera2-fov20.jpg")

