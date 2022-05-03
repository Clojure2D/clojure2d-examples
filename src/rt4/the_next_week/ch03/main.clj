(ns rt4.the-next-week.ch03.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.the-next-week.ch03.sphere :as sphere]
            [rt4.the-next-week.ch03.moving-sphere :as moving-sphere]
            [rt4.the-next-week.ch03.camera :as camera]
            [rt4.the-next-week.ch03.material :as material]
            [rt4.the-next-week.ch03.scene :as scene]
            [rt4.the-next-week.ch03.hittable-list :as hittable-list]
            [rt4.the-next-week.ch03.bvh :as bvh]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; camera

(def camera (camera/camera {:vfov 20.0
                          :lookfrom (v/vec3 13.0 2.0 3.0)
                          :lookat (v/vec3 0.0 0.0 0.0)
                          :vup (v/vec3 0.0 1.0 0.0)
                          :aperture 0.1
                          :focus-dist 10.0}))

(def ground-material (material/lambertian (v/vec3 0.5 0.5 0.5)))
(def material1 (material/dielectric 1.5))
(def material2 (material/lambertian (v/vec3 0.4 0.2 0.1)))
(def material3 (material/metal (v/vec3 0.7 0.6 0.5) 0.0))

(def world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -1000.0 0.0) 1000.0 ground-material)
                                      (sphere/sphere (v/vec3 0.0 1.0 0.0) 1.0 material1)
                                      (sphere/sphere (v/vec3 -4.0 1.0 0.0) 1.0 material2)
                                      (sphere/sphere (v/vec3 4.0 1.0 0.0) 1.0 material3)))

(defn make-balls
  [world]
  (reduce (fn [w [^long a ^long b]]
            (let [choose-mat (r/drand)
                  center (v/vec3 (+ a (r/drand 0.9)) 0.2 (+ b (r/drand 0.9)))]
              (if (> (v/mag (v/sub center (v/vec3 4.0 0.2 0.0))) 0.9)
                (hittable-list/add w (cond
                                       (< choose-mat 0.8)
                                       (let [albedo (v/emult (common/random-vec3)
                                                             (common/random-vec3))
                                             center2 (v/add center (v/vec3 0.0 (r/drand 0.5) 0.0))]
                                         (->> (material/lambertian albedo)
                                              (moving-sphere/sphere center center2 0.2)))
                                       
                                       (< choose-mat 0.95)
                                       (let [albedo (common/random-vec3 0.5 1.0)
                                             fuzz (r/drand 0.5)]
                                         (->> (material/metal albedo fuzz)
                                              (sphere/sphere center 0.2)))
                                       
                                       :else (->> (material/dielectric 1.5)
                                                  (sphere/sphere center 0.2))))
                w)))
          world (for [a (range -11 11) b (range -11 11)] [a b])))

(def scene (scene/scene camera (-> world make-balls bvh/bvh-node) {:image-width 400
                                                                 :samples-per-pixel 100}))

(def image (time (scene/render scene)))

(comment
  (common/save image "results/rt4/the_next_week/ch03.jpg"))
