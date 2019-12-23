(ns rt-in-weekend.ch5-sphere
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/mult (v/vec3 1.0 1.0 1.0) 255.0))
(def ^:const v2 (v/mult (v/vec3 0.5 0.7 1.0) 255.0))
(def ^:const lower-left-corner (v/vec3 -2.0 -1.0 -1.0))
(def ^:const horizontal (v/vec3 4.0 0.0 0.0))
(def ^:const vertical (v/vec3 0.0 2.0 0.0))
(def ^:const orig (v/vec3 0.0 0.0 0.0))
(def ^:const center (v/vec3 0.0 0.0 -1.0))
(def ^:const one (v/vec3 1.0 1.0 1.0))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)

(defn hit-sphere ^double [center ^double radius ^Ray ray]
  (let [oc (v/sub (.origin ray) center)
        a (v/magsq (.direction ray))
        b (* 2.0 (v/dot oc (.direction ray)))
        c (- (v/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      -1.0
      (/ (- (- b) (m/sqrt discriminant)) (+ a a)))))

(defn color [^Ray ray]
  (let [t (hit-sphere center 0.5 ray)]
    (if (pos? t)
      (-> (point-at-parameter ray t)
          (v/sub center)
          (v/normalize)
          (v/add one)
          (v/mult 127.5))
      (let [^Vec3 unit (v/normalize (.direction ray))
            t (* 0.5 (inc (.y unit)))]
        (v/interpolate v1 v2 t)))))

(def img (p/pixels nx ny))

(dotimes [j ny]
  (dotimes [i nx]
    (let [u (/ (double i) nx)
          v (/ (double j) ny)
          r (->Ray orig (v/add lower-left-corner (v/add (v/mult horizontal u) (v/mult vertical v))))]
      (p/set-color! img i (- (dec ny) j) (color r)))))

(u/show-image img)

;; (save img "results/rt-in-weekend/sphere2.jpg")
