(ns RTinWeekend.hitable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord hit-record [^double t ^Vec3 p ^Vec3 normal])

(defprotocol Hitable
  (hit [object ray t-min t-max]))

(defn hit-list
  "Traverse all scene objects."
  [xs ray t-min t-max]
  (reduce (fn [curr-hit object]
            (if-let [hit-object (hit object ray t-min (or (:t curr-hit) t-max))]
              hit-object
              curr-hit)) nil xs))
