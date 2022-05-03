(ns rt4.in-one-weekend.ch04.main
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch04.ray :as ray])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double aspect-ratio (/ 16.0 9.0))

(def ^:const ^long image-width 400)
(def ^:const ^long image-height (long (/ image-width aspect-ratio)))

;; precompute
(def ^:const ^long image-width- (dec image-width))
(def ^:const ^long image-height- (dec image-height))

;; camera

(def ^:const ^double viewport-height 2.0)
(def ^:const ^double viewport-width (* aspect-ratio viewport-height))
(def ^:const ^double focal-length 1.0)

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

(defn ray-color [r]
  (let [^Vec3 unit-direction (v/normalize (:direction r))
        t (* 0.5 (+ (.y unit-direction) 1.0))]
    (v/interpolate one sky t)))

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
  (common/save image "results/rt4/in_one_weekend/ch04.jpg"))
