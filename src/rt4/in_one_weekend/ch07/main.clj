(ns rt4.in-one-weekend.ch07.main
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch07.hittable :as hittable]
            [rt4.in-one-weekend.ch07.sphere :as sphere]
            [rt4.in-one-weekend.ch07.interval :as interval]
            [rt4.in-one-weekend.ch07.camera :as camera]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long image-width 400)
(def ^:const ^long image-height (long (/ image-width camera/aspect-ratio)))

;; precompute
(def ^:const ^long image-width- (dec image-width))
(def ^:const ^long image-height- (dec image-height))

;; camera
(def camera (camera/camera))

(def ^:const ^long samples-per-pixel 100)

(def world [(sphere/sphere {:center (v/vec3 0.0 0.0 -1.0) :radius 0.5})
          (sphere/sphere {:center (v/vec3 0.0 -100.5 -1.0) :radius 100.0})])

(def image (common/make-pixels-and-show image-width image-height))

(def one (v/vec3 1.0 1.0 1.0))
(def zero (v/vec3 0.0 0.0 0.0))
(def sky (v/vec3 0.5 0.7 1.0))

(defn ray-color [r world]
  (if-let [rec (hittable/hit-list world r (interval/interval 0.0 ##Inf))]
    (v/mult (v/add (:normal rec) one) 0.5)
    (let [^Vec3 unit-direction (v/normalize (:direction r))
          t (* 0.5 (+ (.y unit-direction) 1.0))]
      (v/interpolate one sky t))))

(common/pdotimes [j image-height]
  (dotimes [i image-width]
    (let [pixel-color (-> (reduce v/add zero
                                  (repeatedly samples-per-pixel
                                              #(let [u (/ (+ i (r/drand)) image-width-)
                                                     v (/ (+ j (r/drand)) image-height-)]
                                                 (ray-color (camera/get-ray camera u v) world))))
                          (v/div samples-per-pixel)
                          (c/scale-up))]
      (p/set-color! (:pixels image) i (- image-height- j) pixel-color))))

(comment
  (common/save image "results/rt4/in_one_weekend/ch07.jpg"))
