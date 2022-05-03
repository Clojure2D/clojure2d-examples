(ns rt4.in-one-weekend.ch12b.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch12b.sphere :as sphere]
            [rt4.in-one-weekend.ch12b.camera :as camera]
            [rt4.in-one-weekend.ch12b.material :as material]
            [rt4.in-one-weekend.ch12b.scene :as scene]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; camera
(def camera (camera/camera {;; :vfov 20.0
                          :vfov 90.0
                          :lookfrom (v/vec3 -2.0 2.0 1.0)
                          :lookat (v/vec3 0.0 0.0 -1.0)
                          :vup (v/vec3 0.0 1.0 0.0)}))

(def material-ground (material/lambertian (v/vec3 0.8 0.8 0.0)))
(def material-center (material/lambertian (v/vec3 0.1 0.2 0.5)))
(def material-left (material/dielectric 1.5))
(def material-right (material/metal (v/vec3 0.8 0.6 0.2) 0.0))

(def world [(sphere/sphere (v/vec3 0.0 -100.5 -1.0) 100.0 material-ground)
          (sphere/sphere (v/vec3 0.0 0.0 -1.0) 0.5 material-center)
          (sphere/sphere (v/vec3 -1.0 0.0 -1.0) 0.5 material-left)
          (sphere/sphere (v/vec3 -1.0 0.0 -1.0) -0.4 material-left)
          (sphere/sphere (v/vec3 1.0 0.0 -1.0) 0.5 material-right)])

(def scene (scene/scene camera world {:image-width 400
                                    :samples-per-pixel 100}))

(def image (scene/render scene))

(comment
  (common/save image "results/rt4/in_one_weekend/ch12b.jpg") ;; :vfov 90.0
  (common/save image "results/rt4/in_one_weekend/ch12b2.jpg")) ;; vfov 20.0
