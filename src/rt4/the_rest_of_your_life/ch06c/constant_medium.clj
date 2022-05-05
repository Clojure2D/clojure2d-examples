(ns rt4.the-rest-of-your-life.ch06c.constant-medium
  (:require [fastmath.core :as m]
            [rt4.the-rest-of-your-life.ch06c.hittable :as hittable]
            [rt4.the-rest-of-your-life.ch06c.material :as material]
            [rt4.the-rest-of-your-life.ch06c.interval :as interval]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [rt4.the-rest-of-your-life.ch06c.ray :as ray])
  (:import [rt4.the_rest_of_your_life.ch06c.hittable HitData]
           [rt4.the_rest_of_your_life.ch06c.interval Interval]
           [rt4.the_rest_of_your_life.ch06c.ray Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private arbitrary-normal (v/vec3 1.0 0.0 0.0))

(defrecord ConstantMedium [boundary ^double neg-inv-density phase-function bbox]
  hittable/HittableProto
  (hit [_ r ray-t]
    (when-let [^HitData rec1 (hittable/hit boundary r interval/universe)]
      (when-let [^HitData rec2 (hittable/hit boundary r (interval/interval (+ (.t rec1) 0.0001) ##Inf))]
        (let [rec1-t (max (.mn ^Interval ray-t) (.t rec1))
              rec2-t (min (.mx ^Interval ray-t) (.t rec2))]
          (when (< rec1-t rec2-t)
            (let [rec1-t (max 0.0 rec1-t)
                  ray-length (v/mag (.direction ^Ray r))
                  distance-inside-boundary (* (- rec2-t rec1-t) ray-length)
                  hit-distance (* neg-inv-density (m/log (r/drand)))]
              (when (<= hit-distance distance-inside-boundary)
                (let [t (+ rec1-t (/ hit-distance ray-length))]
                  (hittable/->HitData (ray/at r t) arbitrary-normal phase-function t 0.0 0.0 true))))))))))

(defn constant-medium
  [b ^double d color-or-texture]
  (->ConstantMedium b (- (/ d)) (material/isotropic color-or-texture) (:bbox b)))

