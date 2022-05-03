(ns rt4.the-next-week.ch08b.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.the-next-week.ch08b.sphere :as sphere]
            [rt4.the-next-week.ch08b.moving-sphere :as moving-sphere]
            [rt4.the-next-week.ch08b.camera :as camera]
            [rt4.the-next-week.ch08b.material :as material]
            [rt4.the-next-week.ch08b.scene :as scene]
            [rt4.the-next-week.ch08b.hittable-list :as hittable-list]
            [rt4.the-next-week.ch08b.bvh :as bvh]
            [fastmath.random :as r]
            [rt4.the-next-week.ch08b.texture :as texture]
            [rt4.the-next-week.ch08b.quad :as quad]
            [rt4.the-next-week.ch08b.hittable :as hittable]))

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
                      :samples-per-pixel 100
                      :background (v/vec3 0.7 0.8 1.0)})

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
        checker (texture/checker-texture 0.32 (v/vec3 0.2 0.3 0.1) (v/vec3 0.9 0.9 0.9))
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
        checker (texture/checker-texture 0.32 (v/vec3 0.2 0.3 0.1) (v/vec3 0.9 0.9 0.9))
        material (material/lambertian checker)
        world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -10.0 0.0) 10.0 material)
                                           (sphere/sphere (v/vec3 0.0 10.0 0.0) 10.0 material))]
    (scene/scene camera (-> world bvh/bvh-node) scene-def)))

(defonce earth-texture
  (texture/image-texture
   "https://github.com/RayTracing/raytracing.github.io/raw/master/images/earthmap.jpg"))

(defn earth [scene-def]
  ;; :lookfrom (v/vec3 0.0 0.0 12.0) yield different view than in the book
  (let [camera (camera/camera (assoc default-camera-def :aperture 0.0))
        earth-surface (material/lambertian earth-texture)
        globe (sphere/sphere (v/vec3 0.0 0.0 0.0) 2.0 earth-surface)]
    (scene/scene camera (hittable-list/hittable-list globe) scene-def)))

(defn two-perlin-spheres [scene-def]
  (let [camera (camera/camera (assoc default-camera-def :aperture 0.0))
        pertext (texture/noise-texture 4.0)
        material (material/lambertian pertext)
        world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -1000.0 0.0) 1000.0 material)
                                           (sphere/sphere (v/vec3 0.0 2.0 0.0) 2.0 material))]
    (scene/scene camera (bvh/bvh-node world) scene-def)))


(defn quads [scene-def]
  (let [camera (camera/camera (assoc default-camera-def
                                     :aspect-ratio (:aspect-ratio scene-def)
                                     :aperture 0.0 :vfov 80.0 :lookfrom (v/vec3 0.0 0.0 9.0)))
        left-red (material/lambertian (v/vec3 1.0 0.2 0.2))
        back-green (material/lambertian (v/vec3 0.2 1.0 0.2))
        right-blue (material/lambertian (v/vec3 0.2 0.2 1.0))
        upper-orange (material/lambertian (v/vec3 1.0 0.5 0.0))
        lower-teal (material/lambertian (v/vec3 0.2 0.8 0.8))
        
        world (hittable-list/hittable-list (quad/quad (v/vec3 -3.0 -2.0 5.0) (v/vec3 0.0 0.0 -4.0)
                                                      (v/vec3 0.0 4.0 0.0) left-red)
                                           (quad/quad (v/vec3 -2.0 -2.0 0.0) (v/vec3 4.0 0.0 0.0)
                                                      (v/vec3 0.0 4.0 0.0) back-green)
                                           (quad/quad (v/vec3 3.0 -2.0 1.0) (v/vec3 0.0 0.0 4.0)
                                                      (v/vec3 0.0 4.0 0.0) right-blue)
                                           (quad/quad (v/vec3 -2.0 3.0 1.0) (v/vec3 4.0 0.0 0.0)
                                                      (v/vec3 0.0 0.0 4.0) upper-orange)
                                           (quad/quad (v/vec3 -2.0 -3.0 5.0) (v/vec3 4.0 0.0 0.0)
                                                      (v/vec3 0.0 0.0 -4.0) lower-teal))]
    (scene/scene camera (bvh/bvh-node world) scene-def)))

(defn simple-light [scene-def]
  (let [camera (camera/camera (assoc default-camera-def
                                     :aperture 0.0 :lookfrom (v/vec3 26.0 3.0 6.0)
                                     :lookat (v/vec3 0.0 2.0 0.0)))
        pertext (texture/noise-texture 4.0)
        material (material/lambertian pertext)
        difflight (material/diffuse-light (v/vec3 4.0 4.0 4.0))
        world (hittable-list/hittable-list (sphere/sphere (v/vec3 0.0 -1000.0 0.0) 1000.0 material)
                                           (sphere/sphere (v/vec3 0.0 2.0 0.0) 2.0 material)
                                           (sphere/sphere (v/vec3 0.0 7.0 0.0) 2.0 difflight)
                                           (quad/quad (v/vec3 3.0 1.0 -2.0) (v/vec3 2.0 0.0 0.0)
                                                      (v/vec3 0.0 2.0 0.0) difflight))]
    (scene/scene camera (bvh/bvh-node world) scene-def)))

(defn cornell-box [scene-def]
  (let [camera (camera/camera (assoc default-camera-def
                                     :aspect-ratio (:aspect-ratio scene-def)
                                     :aperture 0.0 :vfov 40.0 :lookfrom (v/vec3 278.0 278.0 -800.0)
                                     :lookat (v/vec3 278.0 278.0 0.0)))
        red (material/lambertian (v/vec3 0.65 0.05 0.05))
        white (material/lambertian (v/vec3 0.73 0.73 0.73))
        green (material/lambertian (v/vec3 0.12 0.45 0.15))
        light (material/diffuse-light (v/vec3 15.0 15.0 15.0))

        box1 (-> (quad/box (v/vec3 0.0 0.0 0.0) (v/vec3 165 330 165) white)
                 (hittable/rotate-y 15.0)
                 (hittable/translate (v/vec3 265 0 295)))
        box2 (-> (quad/box (v/vec3 0.0 0.0 0.0) (v/vec3 165 165 165) white)
                 (hittable/rotate-y -18.0)
                 (hittable/translate (v/vec3 130 0 65)))
        
        world (hittable-list/hittable-list (quad/quad (v/vec3 555 0 0) (v/vec3 0 555 0)
                                                      (v/vec3 0 0 555) green)
                                           (quad/quad (v/vec3 0 0 0) (v/vec3 0 555 0)
                                                      (v/vec3 0 0 555) red)
                                           (quad/quad (v/vec3 343 554 332) (v/vec3 -130 0 0)
                                                      (v/vec3 0 0 -105) light)
                                           (quad/quad (v/vec3 0 0 0) (v/vec3 555 0 0)
                                                      (v/vec3 0 0 555) white)
                                           (quad/quad (v/vec3 555 555 555) (v/vec3 -555 0 0)
                                                      (v/vec3 0 0 -555) white)
                                           (quad/quad (v/vec3 0 0 555) (v/vec3 555 0 0)
                                                      (v/vec3 0 555 0) white)
                                           box1 box2)]
    (scene/scene camera (bvh/bvh-node world) scene-def)))

(defn main
  ([] (main 0))
  ([^long scene] (main default-scene-def scene))
  ([scene-def ^long scene]
   (let [scene-def (merge default-scene-def scene-def)
         f (case scene
             1 random-spheres
             2 two-spheres
             3 earth
             4 two-perlin-spheres
             5 quads
             6 simple-light
             7 cornell-box
             cornell-box)]
     (f scene-def))))

(def image (time (scene/render (main {:background (v/vec3 0.0 0.0 0.0)
                                    :samples-per-pixel 200
                                    :aspect-ratio 1.0
                                    :image-width 600} 0))))

(comment
  (common/save image "results/rt4/the_next_week/ch08b.jpg"))

