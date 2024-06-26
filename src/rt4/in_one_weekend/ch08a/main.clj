(ns rt4.in-one-weekend.ch08a.main
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch08a.hittable :as hittable]
            [rt4.in-one-weekend.ch08a.sphere :as sphere]
            [rt4.in-one-weekend.ch08a.interval :as interval]
            [rt4.in-one-weekend.ch08a.camera :as camera]
            [rt4.in-one-weekend.ch08a.ray :as ray]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const image-width 400)
(def ^:const image-height (long (/ image-width camera/aspect-ratio)))

;; precompute
(def ^:const image-width- (dec image-width))
(def ^:const image-height- (dec image-height))

;; camera
(def camera (camera/camera))

(def ^:const samples-per-pixel 100)
(def ^:const max-depth 50)

(def world [(sphere/sphere {:center (v/vec3 0.0 0.0 -1.0) :radius 0.5})
          (sphere/sphere {:center (v/vec3 0.0 -100.5 -1.0) :radius 100.0})])

(def image (common/make-pixels-and-show image-width image-height))

(def one (v/vec3 1.0 1.0 1.0))
(def zero (v/vec3 0.0 0.0 0.0))
(def sky (v/vec3 0.5 0.7 1.0))

(defn ray-color [r world ^long depth]
  (if (zero? depth)
    zero
    (if-let [rec (hittable/hit-list world r (interval/interval 0.0 ##Inf))]
      (let [direction (common/random-on-hemisphere (:normal rec))]
        (v/mult (ray-color (ray/ray (:p rec) direction) world (dec depth)) 0.5))
      (let [^Vec3 unit-direction (v/normalize (:direction r))
            t (* 0.5 (+ (.y unit-direction) 1.0))]
        (v/interpolate one sky t)))))

(common/pdotimes [j image-height]
  (when (common/active? image)
    (dotimes [i image-width]
      (let [pixel-color (-> (reduce v/add zero
                                    (repeatedly samples-per-pixel
                                                #(let [u (/ (+ i (r/drand)) image-width-)
                                                       v (/ (+ j (r/drand)) image-height-)]
                                                   (ray-color (camera/get-ray camera u v) world max-depth))))
                            (v/div samples-per-pixel)
                            (c/scale-up))]
        (p/set-color! (:pixels image) i (- image-height- j) pixel-color)))))

(comment
  (common/save image "results/rt4/in_one_weekend/ch08a.jpg"))
