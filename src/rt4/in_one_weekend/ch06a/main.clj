(ns rt4.in-one-weekend.ch06a.main
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch06a.ray :as ray])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const aspect-ratio (/ 16.0 9.0))

(def ^:const image-width 400)
(def ^:const image-height (long (/ image-width aspect-ratio)))

;; precompute
(def ^:const image-width- (dec image-width))
(def ^:const image-height- (dec image-height))

;; camera

(def ^:const viewport-height 2.0)
(def ^:const viewport-width (* aspect-ratio viewport-height))
(def ^:const focal-length 1.0)

(def origin (v/vec3 0.0 0.0 0.0))
(def horizontal (v/vec3 viewport-width 0.0 0.0))
(def vertical (v/vec3 0.0 viewport-height 0.0))
(def lower-left-corner (-> origin
                         (v/sub (v/div horizontal 2.0))
                         (v/sub (v/div vertical 2.0))
                         (v/sub (v/vec3 0.0 0.0 focal-length))))

(def image (common/make-pixels-and-show image-width image-height))

(def one (v/vec3 1.0 1.0 1.0))
(def sky (v/vec3 0.5 0.7 1.0))

(defn hit-sphere
  ^double [center ^double radius r]
  (let [oc (v/sub (:origin r) center)
        a (v/dot (:direction r) (:direction r))
        b (* 2.0 (v/dot oc (:direction r)))
        c (- (v/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* 4.0 a c))]
    (if (neg? discriminant)
      -1.0
      (/ (- (- b) (m/sqrt discriminant)) (* 2.0 a)))))

(def sphere-position (v/vec3 0.0 0.0 -1.0))

(defn ray-color [r]
  (let [t (hit-sphere sphere-position 0.5 r)]
    (if (pos? t)
      (let [N (v/normalize (v/sub (ray/at r t) sphere-position))]
        (v/mult (v/add N one) 0.5))
      (let [^Vec3 unit-direction (v/normalize (:direction r))
            t (* 0.5 (+ (.y unit-direction) 1.0))]
        (v/interpolate one sky t)))))

(common/pdotimes [j image-height]
  (dotimes [i image-width]
    (let [u (/ (double i) image-width-)
          v (/ (double j) image-height-)
          r (ray/ray origin (-> lower-left-corner
                                (v/add (v/mult horizontal u))
                                (v/add (v/mult vertical v))
                                (v/sub origin)))]
      (p/set-color! (:pixels image) i (- image-height- j) (c/scale-up (ray-color r))))))

(comment
  (common/save image "results/rt4/in_one_weekend/ch06a.jpg"))
