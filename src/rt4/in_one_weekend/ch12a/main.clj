(ns rt4.in-one-weekend.ch12a.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch12a.sphere :as sphere]
            [rt4.in-one-weekend.ch12a.camera :as camera]
            [rt4.in-one-weekend.ch12a.material :as material]
            [rt4.in-one-weekend.ch12a.scene :as scene]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; camera
(def camera (camera/camera {:vfov 90.0}))

(def material-left (material/lambertian (v/vec3 0.0 0.0 1.0)))
(def material-right (material/lambertian (v/vec3 1.0 0.0 0.0)))

(def ^:const ^double R (m/cos m/QUARTER_PI))

(def world [(sphere/sphere (v/vec3 (- R) 0.0 -1.0) R material-left)
          (sphere/sphere (v/vec3 R 0.0 -1.0) R material-right)])

(def scene (scene/scene camera world {:image-width 400
                                    :samples-per-pixel 100}))

(def image (scene/render scene))

(comment
  (common/save image "results/rt4/in_one_weekend/ch12a.jpg"))
