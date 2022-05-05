(ns rt4.the-rest-of-your-life.ch06b.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.the-rest-of-your-life.ch06b.camera :as camera]
            [rt4.the-rest-of-your-life.ch06b.material :as material]
            [rt4.the-rest-of-your-life.ch06b.scene :as scene]
            [rt4.the-rest-of-your-life.ch06b.hittable-list :as hittable-list]
            [rt4.the-rest-of-your-life.ch06b.bvh :as bvh]
            [rt4.the-rest-of-your-life.ch06b.quad :as quad]
            [rt4.the-rest-of-your-life.ch06b.hittable :as hittable]))

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


(defn cornell-box [scene-def]
  (let [camera (camera/camera (assoc default-camera-def
                                     :aspect-ratio (:aspect-ratio scene-def)
                                     :aperture 0.0 :vfov 40.0 :lookfrom (v/vec3 278.0 278.0 -800.0)
                                     :focus-dist 10.0
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
                                           (quad/quad (v/vec3 0 0 555) (v/vec3 0 0 -555)
                                                      (v/vec3 0 555 0) red)
                                           (quad/quad (v/vec3 0 555 0) (v/vec3 555 0 0)
                                                      (v/vec3 0 0 555) white)
                                           (quad/quad (v/vec3 0 0 555) (v/vec3 555 0 0)
                                                      (v/vec3 0 0 -555) white)
                                           (quad/quad (v/vec3 555 0 555) (v/vec3 -555 0 0)
                                                      (v/vec3 0 555 0) white)
                                           box1 box2
                                           (quad/quad (v/vec3 213 554 227) (v/vec3 130 0 0)
                                                      (v/vec3 0 0 105) light))]
    (scene/scene camera (bvh/bvh-node world) scene-def)))

(defn main
  ([] (main default-scene-def))
  ([scene-def]
   (cornell-box (merge default-scene-def scene-def))))

(def image (time (scene/render (main {:background (v/vec3 0.0 0.0 0.0)
                                    :samples-per-pixel 100
                                    :aspect-ratio 1.0
                                    :shuffle? false
                                    :max-depth 50
                                    :image-width 500}))))

(comment
  (common/save image "results/rt4/the_rest_of_your_life/ch06b.jpg"))

