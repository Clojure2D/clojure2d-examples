(ns RTinWeekend.ch6-antialiasing
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

(def ^:const v1 (v/mult (v/vec3 1.0 1.0 1.0) 255.0))
(def ^:const v2 (v/mult (v/vec3 0.5 0.7 1.0) 255.0))
(def ^:const one (v/vec3 1.0 1.0 1.0))

(def world [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5)
            (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0)])

(defn color [ray world]
  (if-let [world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
    (v/mult (v/add (:normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (:direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 150)

(def img (p/pixels nx ny))

(dotimes [j ny]
  (println (str "Line: " j))
  (dotimes [i nx]
    (let [col (reduce v/add (v/vec3 0.0 0.0 0.0)
                      (repeatedly samples #(let [u (/ (+ (r/drand) (double i)) nx)
                                                 v (/ (+ (r/drand) (double j)) ny)
                                                 r (get-ray default-camera u v)]
                                             (color r world))))]
      (p/set-color img i (- (dec ny) j) (v/div col samples)))))

(u/show-image img)

(save img "results/RTinWeekend/antialiasing.jpg")
