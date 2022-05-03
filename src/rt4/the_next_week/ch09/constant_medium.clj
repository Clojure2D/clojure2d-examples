(ns rt4.the-next-week.ch09.constant-medium
  (:require [fastmath.core :as m]
            [rt4.the-next-week.ch09.hittable :as hittable]
            [rt4.the-next-week.ch09.material :as material]
            [rt4.the-next-week.ch09.interval :as interval]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [rt4.the-next-week.ch09.ray :as ray])
  (:import [rt4.the_next_week.ch09.hittable HitData]
           [rt4.the_next_week.ch09.interval Interval]))

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
                  ray-length (v/mag (:direction r))
                  distance-inside-boundary (* (- rec2-t rec1-t) ray-length)
                  hit-distance (* neg-inv-density (m/log (r/drand)))]
              (when (<= hit-distance distance-inside-boundary)
                (let [t (+ rec1-t (/ hit-distance ray-length))]
                  (hittable/->HitData (ray/at r t) arbitrary-normal phase-function t 0.0 0.0 true))))))))))

(defn constant-medium
  [b ^double d color-or-texture]
  (->ConstantMedium b (- (/ d)) (material/isotropic color-or-texture) (:bbox b)))

