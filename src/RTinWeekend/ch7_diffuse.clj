(ns RTinWeekend.ch7-diffuse
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [RTinWeekend.ray :refer :all]
            [RTinWeekend.hitable :refer :all]
            [RTinWeekend.sphere :refer :all]
            [RTinWeekend.camera :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))
(def ^:const one (v/vec3 1.0 1.0 1.0))

(def world [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5)
            (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0)])

(defn random-in-unit-sphere []
  (let [v (v/vec3 (r/drand -1.0 1.0) (r/drand -1.0 1.0) (r/drand -1.0 1.0))]
    (if (< ^double (v/magsq v) 1.0) v (recur))))

(defn color [ray world]
  (if-let [world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
    (let [target (v/add (v/add (random-in-unit-sphere) (:normal world-hit)) (:p world-hit))]
      (v/mult (color (->Ray (:p world-hit) (v/sub target (:p world-hit))) world) 0.5))
    (let [^Vec3 unit (v/normalize (:direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 200)

(def img (p/pixels nx ny))

(dotimes [j ny]
  (println (str "Line: " j))
  (dotimes [i nx]
    (let [col (reduce v/add (v/vec3 0.0 0.0 0.0)
                      (repeatedly samples #(let [u (/ (+ (r/drand) (double i)) nx)
                                                 v (/ (+ (r/drand) (double j)) ny)
                                                 r (get-ray default-camera u v)]
                                             (color r world))))]
      (p/set-color img i (- (dec ny) j) (-> (v/div col samples)
                                            (v/applyf #(m/sqrt %))
                                            (v/mult 255.0))))))

(u/show-image img)

(save img "results/RTinWeekend/diffuse.jpg")
