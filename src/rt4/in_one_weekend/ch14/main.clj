(ns rt4.in-one-weekend.ch14.main
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.common :as common]
            [rt4.in-one-weekend.ch14.sphere :as sphere]
            [rt4.in-one-weekend.ch14.camera :as camera]
            [rt4.in-one-weekend.ch14.material :as material]
            [rt4.in-one-weekend.ch14.scene :as scene]
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

(def camera-low (camera/camera {:vfov 21.0
                              :lookfrom (v/vec3 13.0 5.0 5.0)
                              :lookat (v/vec3 0.0 0.0 0.0)
                              :vup (v/vec3 0.0 1.0 0.0)
                              :aperture 0.1
                              :aspect-ratio 1.5
                              :focus-dist 10.0}))


(def ground-material (material/lambertian (v/vec3 0.5 0.5 0.5)))
(def material1 (material/dielectric 1.5))
(def material2 (material/lambertian (v/vec3 0.4 0.2 0.1)))
(def material3 (material/metal (v/vec3 0.7 0.6 0.5) 0.0))

(def world [(sphere/sphere (v/vec3 0.0 -1000.0 0.0) 1000.0 ground-material)
          (sphere/sphere (v/vec3 0.0 1.0 0.0) 1.0 material1)
          (sphere/sphere (v/vec3 -4.0 1.0 0.0) 1.0 material2)
          (sphere/sphere (v/vec3 4.0 1.0 0.0) 1.0 material3)])

(defn make-balls
  [world]
  (reduce (fn [w [^long a ^long b]]
            (let [choose-mat (r/drand)
                  center (v/vec3 (+ a (r/drand 0.9)) 0.2 (+ b (r/drand 0.9)))]
              (if (> (v/mag (v/sub center (v/vec3 4.0 0.2 0.0))) 0.9)
                (conj w (sphere/sphere center 0.2
                                       (cond
                                         (< choose-mat 0.8) (let [albedo (v/emult (common/random-vec3)
                                                                                  (common/random-vec3))]
                                                              (material/lambertian albedo))
                                         (< choose-mat 0.95) (let [albedo (common/random-vec3 0.5 1.0)
                                                                   fuzz (r/drand 0.5)]
                                                               (material/metal albedo fuzz))
                                         :else (material/dielectric 1.5))))
                w)))
          world (for [a (range -11 11) b (range -11 11)] [a b])))

#_(def scene (scene/scene camera (make-balls world) {:image-width 1200
                                                   :samples-per-pixel 200}))

(def scene-low (scene/scene camera-low (make-balls world) {:image-width 400
                                                         :samples-per-pixel 10
                                                         :max-depth 10
                                                         :aspect-ratio 1.5}))


#_(def scene-mid (scene/scene camera (make-balls world) {:image-width 800
                                                       :samples-per-pixel 200}))


#_(def image (time (scene/render scene)))
#_(def image (time (scene/render scene-mid)))
(def image-low (time (scene/render scene-low)))

(comment
  (common/save image "results/rt4/in_one_weekend/ch14.jpg")
  #_(common/save image "results/rt4/in_one_weekend/ch14mid.jpg"))
