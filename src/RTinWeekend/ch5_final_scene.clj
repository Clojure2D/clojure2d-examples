(ns RTinWeekend.ch5-final-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [RTinWeekend.ray :refer :all]
            [RTinWeekend.hitable :refer :all]
            [RTinWeekend.sphere :refer :all]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/mult (v/vec3 1.0 1.0 1.0) 255.0))
(def ^:const v2 (v/mult (v/vec3 0.5 0.7 1.0) 255.0))
(def ^:const lower-left-corner (v/vec3 -2.0 -1.0 -1.0))
(def ^:const horizontal (v/vec3 4.0 0.0 0.0))
(def ^:const vertical (v/vec3 0.0 2.0 0.0))
(def ^:const orig (v/vec3 0.0 0.0 0.0))
(def ^:const one (v/vec3 1.0 1.0 1.0))

(def world [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5)
            (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0)])

(defn color [ray world]
  (if-let [world-hit (hit-list world ray 0.0 Double/MAX_VALUE)]
    (v/mult (v/add (:normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (:direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)

(def img (p/pixels nx ny))

(dotimes [j ny]
  (dotimes [i nx]
    (let [u (/ (double i) nx)
          v (/ (double j) ny)
          r (->Ray orig (v/add lower-left-corner (v/add (v/mult horizontal u) (v/mult vertical v))))]
      (p/set-color img i (- (dec ny) j) (color r world)))))

(u/show-image img)

(save img "results/RTinWeekend/scene.jpg")
