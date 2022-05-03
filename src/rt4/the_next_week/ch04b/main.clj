(ns rt4.the-next-week.ch04b.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.the-next-week.ch04b.sphere :as sphere]
            [rt4.the-next-week.ch04b.moving-sphere :as moving-sphere]
            [rt4.the-next-week.ch04b.camera :as camera]
            [rt4.the-next-week.ch04b.material :as material]
            [rt4.the-next-week.ch04b.scene :as scene]
            [rt4.the-next-week.ch04b.hittable-list :as hittable-list]
            [rt4.the-next-week.ch04b.bvh :as bvh]
            [fastmath.random :as r]
            [rt4.the-next-week.ch04b.texture :as texture]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; camera

(def default-camera-def {:vfov 20.0
                       :lookfrom (v/vec3 13.0 2.0 3.0)
                       :lookat (v/vec3 0.0 0.0 0.0)
                       :vup (v/vec3 0.0 1.0 0.0)
                       :aperture 0.1
                       :focus-dist 10.0})

(def default-scene-def {:image-width 400
                      :samples-per-pixel 100})

(defn- make-balls
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

(defn random-spheres [scene-def]
  (let [camera (camera/camera default-camera-def)
        checker (texture/color-checker-texture 0.32 (v/vec3 0.2 0.3 0.1) (v/vec3 0.9 0.9 0.9))
        ground-material (material/lambertian checker)
        material1 (material/dielectric 1.5)
        material2 (material/lambertian (v/vec3 0.4 0.2 0.1))
        material3 (material/metal (v/vec3 0.7 0.6 0.5) 0.0)
        world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -1000.0 0.0) 1000.0 ground-material)
                                           (sphere/sphere (v/vec3 0.0 1.0 0.0) 1.0 material1)
                                           (sphere/sphere (v/vec3 -4.0 1.0 0.0) 1.0 material2)
                                           (sphere/sphere (v/vec3 4.0 1.0 0.0) 1.0 material3))]
    (scene/scene camera (-> world make-balls bvh/bvh-node) scene-def)))


(defn two-spheres [scene-def]
  (let [camera (camera/camera (assoc default-camera-def :aperture 0.0))
        checker (texture/color-checker-texture 0.32 (v/vec3 0.2 0.3 0.1) (v/vec3 0.9 0.9 0.9))
        material (material/lambertian checker)
        world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -10.0 0.0) 10.0 material)
                                           (sphere/sphere (v/vec3 0.0 10.0 0.0) 10.0 material))]
    (scene/scene camera (-> world bvh/bvh-node) scene-def)))

(defn main
  ([] (main 0))
  ([^long scene] (main default-scene-def scene))
  ([scene-def ^long scene]
   (let [scene-def (merge default-scene-def scene-def)]
     (case scene
       1 (random-spheres scene-def)
       2 (two-spheres scene-def)
       (two-spheres scene-def)))))

(def image (time (scene/render (main))))

(comment
  (common/save image "results/rt4/the_next_week/ch04b.jpg"))
