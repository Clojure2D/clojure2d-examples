(ns rt-in-weekend.ch5-final-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

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

(def world [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5 nil)
            (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 nil)])

(defn color [^Ray ray world]
  (if-let [^HitData world-hit (hit-list world ray 0.0 Double/MAX_VALUE)]
    (v/mult (v/add (.normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (.direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)

(def img (p/pixels nx ny))

(time (dotimes [j ny]
        (dotimes [i nx]
          (let [u (/ (double i) nx)
                v (/ (double j) ny)
                r (->Ray orig (v/add lower-left-corner (v/add (v/mult horizontal u) (v/mult vertical v))))]
            (p/set-color! img i (- (dec ny) j) (color r world))))))

(u/show-image img)

;; (save img "results/rt-in-weekend/scene.jpg")
