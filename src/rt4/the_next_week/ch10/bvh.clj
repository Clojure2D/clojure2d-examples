(ns rt4.the-next-week.ch10.bvh
  (:require [rt4.the-next-week.ch10.hittable :as hittable]
            [rt4.the-next-week.ch10.aabb :as aabb]
            [rt4.the-next-week.ch10.interval :as interval]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord BVHNode [left right bbox]
  hittable/HittableProto
  (hit [_ r ray-t]
    (when (aabb/hit bbox r ray-t)
      (let [rec-left (and left (hittable/hit left r ray-t))
            rec-right (and right (hittable/hit right r (interval/interval (:mn ray-t)
                                                                          (or (:t rec-left)
                                                                              (:mx ray-t)))))]
        (or rec-right rec-left)))))

(defmacro ^:private compare-hittables
  [selector]
  `(fn [a# b#]
     (< (double (:mn (~selector (:bbox a#))))
        (double (:mn (~selector (:bbox b#)))))))

(def comparators [(compare-hittables :x)
                (compare-hittables :y)
                (compare-hittables :z)])

(defn- build-tree
  [[a b :as objects]]
  (let [comparator (rand-nth comparators)
        object-span (count objects) 
        [left right] (condp = object-span
                       1 [nil a]
                       2 (if (comparator a b) [a b] [b a])
                       (let [sorted-objects (sort comparator objects)
                             mid (/ object-span 2)]
                         (map build-tree (split-at mid sorted-objects))))]
    (->BVHNode left right (if left (aabb/merge-boxes (:bbox left) (:bbox right)) (:bbox right)))))

(defn bvh-node
  ([hittable-list] (build-tree (:objects hittable-list))))

