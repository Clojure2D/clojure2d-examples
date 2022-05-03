(ns rt4.in-one-weekend.ch10a.main
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch10a.hittable :as hittable]
            [rt4.in-one-weekend.ch10a.sphere :as sphere]
            [rt4.in-one-weekend.ch10a.interval :as interval]
            [rt4.in-one-weekend.ch10a.camera :as camera]
            [rt4.in-one-weekend.ch10a.material :as material]
            [rt4.color :as color]
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
(def ^:const ^long max-depth 50)

(def material-ground (material/lambertian (v/vec3 0.8 0.8 0.0)))
(def material-center (material/dielectric 1.5))
(def material-left (material/dielectric 1.5))
(def material-right (material/metal (v/vec3 0.8 0.6 0.2) 1.0))

(def world [(sphere/sphere (v/vec3 0.0 -100.5 -1.0) 100.0 material-ground)
          (sphere/sphere (v/vec3 0.0 0.0 -1.0) 0.5 material-center)
          (sphere/sphere (v/vec3 -1.0 0.0 -1.0) 0.5 material-left)
          (sphere/sphere (v/vec3 1.0 0.0 -1.0) 0.5 material-right)])

(def image (common/make-pixels-and-show image-width image-height))

(def one (v/vec3 1.0 1.0 1.0))
(def zero (v/vec3 0.0 0.0 0.0))
(def sky (v/vec3 0.5 0.7 1.0))

(defn ray-color [r world ^long depth]
  (if (zero? depth)
    zero
    (if-let [rec (hittable/hit-list world r (interval/interval 0.001 ##Inf))]
      (if-let [scatter (material/scatter (:mat rec) r rec)]
        (v/emult (:attenuation scatter) (ray-color (:scattered scatter) world (dec depth)))
        zero)
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
                            (color/->color samples-per-pixel))]
        (p/set-color! (:pixels image) i (- image-height- j) pixel-color)))))

(comment
  (common/save image "results/rt4/in_one_weekend/ch10a.jpg"))
