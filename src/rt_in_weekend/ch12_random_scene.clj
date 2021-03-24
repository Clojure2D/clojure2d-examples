(ns rt-in-weekend.ch12-random-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.camera :refer :all]
            [rt-in-weekend.material :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3 Vec2]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(def ^:const zero (v/vec3 0.0 0.0 0.0))

(defn random-scene []
  (let [world [(->Sphere (v/vec3 0 -1000 0) 1000 (->Lambertian (v/vec3 0.5 0.5 0.5)))
               (->Sphere (v/vec3 0 1 0) 1.0 (->Dielectric 1.5))
               (->Sphere (v/vec3 -4 1 0.5) 1.0 (->Lambertian (v/vec3 0.4 0.2 0.1)))
               (->Sphere (v/vec3 4 1 -0.5) 1.0 (->Metal (v/vec3 0.7 0.6 0.5) 0.0))]]
    (vec (concat world (for [^int a (range -11 11)
                             ^int b (range -11 11)
                             :let [center (v/vec3 (+ a (r/drand 0.9)) 0.2 (+ b (r/drand 0.9)))
                                   choose-mat (r/drand)]
                             :when (> (v/mag (v/sub center (v/vec3 4 0.2 0))) 0.9)]
                         (->Sphere center 0.2 (cond
                                                (< choose-mat 0.6) (->Lambertian (v/vec3 (m/sq (r/drand))
                                                                                         (m/sq (r/drand))
                                                                                         (m/sq (r/drand))))
                                                (< choose-mat 0.9) (->Metal (v/vec3 (r/drand 0.5 1.0)
                                                                                    (r/drand 0.5 1.0)
                                                                                    (r/drand 0.5 1.0)) (r/drand))
                                                :else (->Dielectric (r/drand 1 2)))))))))

(defn color
  ([ray world] (color ray world 50))
  ([^Ray ray world ^long depth]
   (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
     (let [[attenuation scattered] (scatter (.material world-hit) ray world-hit)]
       (if (and attenuation (pos? depth))
         (v/emult attenuation (color scattered world (dec depth)))
         zero))
     (let [^Vec3 unit (v/normalize (.direction ray))
           t (* 0.5 (inc (.y unit)))]
       (v/interpolate v1 v2 t)))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^double dnx (/ 800.0))
(def ^:const ^double dny (/ 400.0))
(def ^:const ^int samples 200)

(def img (p/pixels nx ny))

(def camera
  (let [lookfrom (v/vec3 13 2 4)
        lookat (v/vec3 0 0 0)]
    (positionable-camera lookfrom lookat (v/vec3 0 1 0) 20 (/ (double nx) ny) 0.1 10.0)))

(def world (random-scene))
(def r2-seq (vec (take samples (r/jittered-sequence-generator :r2 2 0.5))))

(def window (show-window {:canvas (canvas nx ny)
                          :draw-fn (fn [c _ f _] (p/set-canvas-pixels! c img))
                          :fps 1}))

(time (dotimes [j ny]
        (when (window-active? window)
          (println (str "Line: " j))
          (dotimes [i nx]
            (let [p (v/vec2 i j)
                  col (reduce v/add zero
                              (pmap #(let [^Vec2 pp (v/add % p)
                                           u (* (.x pp) dnx)
                                           v (* (.y pp) dny)
                                           r (get-ray camera u v)]
                                       (color r world)) r2-seq))]
              (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                     (v/sqrt)
                                                     (v/mult 255.0))))))))

;; (save img "results/rt-in-weekend/random_scene_r2.jpg")
