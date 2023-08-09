(ns rt4.the-rest-of-your-life.ch08.aabb
  (:require [rt4.the-rest-of-your-life.ch08.interval :as interval]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [rt4.the-rest-of-your-life.ch08.ray])
  (:import [fastmath.vector Vec3]
           [rt4.the_rest_of_your_life.ch08.interval Interval]
           [rt4.the_rest_of_your_life.ch08.ray Ray]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol AABBProto
  (axis [aabb n])
  (hit [aabb r ray-t]))

(defmacro ^:private check-axis
  [i d o]
  `(let [invd# (/ ~d)
         m0# (if (neg? invd#) (.mx ~i) (.mn ~i))
         m1# (if (neg? invd#) (.mn ~i) (.mx ~i))
         t0# (* (- m0# ~o) invd#)
         t1# (* (- m1# ~o) invd#)
         ray-tmin# (if (> t0# ~'rt-min) t0# ~'rt-min)
         ray-tmax# (if (< t1# ~'rt-max) t1# ~'rt-max)]
     (> ray-tmax# ray-tmin#)))

(defrecord AABB [^Interval x ^Interval y ^Interval z]
  AABBProto
  (axis [_ n] (case (int n) 0 x 1 y 2 z))
  (hit [_ r ray-t]
    (let [^Vec3 direction (.direction ^Ray r)
          ^Vec3 origin (.origin ^Ray r)
          rt-min (.mn ^Interval ray-t)
          rt-max (.mx ^Interval ray-t)]
      (and (check-axis x (.x direction) (.x origin))
           (check-axis y (.y direction) (.y origin))
           (check-axis z (.z direction) (.z origin))))))

(defn aabb
  (^AABB [^Vec3 a ^Vec3 b]
   (aabb (interval/interval (min (.x a) (.x b)) (max (.x a) (.x b)))
         (interval/interval (min (.y a) (.y b)) (max (.y a) (.y b)))
         (interval/interval (min (.z a) (.z b)) (max (.z a) (.z b)))))
  (^AABB [x y z] (->AABB x y z)))

(defn merge-boxes
  ^AABB [^AABB box0 ^AABB box1]
  (aabb (interval/merge-intervals (:x box0) (:x box1))
        (interval/merge-intervals (:y box0) (:y box1))
        (interval/merge-intervals (:z box0) (:z box1))))

(def ^:private ^:const ^double delta 0.0001)

(defn pad
  ^AABB [^AABB box]
  (let [x (.x box)
        y (.y box)
        z (.z box)]
    (->AABB (if (>= (interval/size x) delta) x (interval/expand x delta))
            (if (>= (interval/size y) delta) y (interval/expand y delta))
            (if (>= (interval/size z) delta) z (interval/expand z delta)))))

(defn shift
  ^AABB [^AABB aabb ^Vec3 offset]
  (->AABB (interval/shift (.x aabb) (.x offset))
          (interval/shift (.y aabb) (.y offset))
          (interval/shift (.z aabb) (.z offset))))
