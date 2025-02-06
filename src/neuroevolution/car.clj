(ns neuroevolution.car
  (:require [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [neuroevolution.noise :as n])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const MAX-VELOCITY 15.0)
(def ^:const MAX-ACCELERATION-F 1.0)
(def ^:const MAX-ACCELERATION-B 0.4)
(def ^:const MAX-ROTATION 0.05)
(def ^:const FRICTION 0.92)

(defrecord Car [position velocity ^double angle direction])

(defn car
  ([position] (car position (r/drand m/TWO_PI)))
  ([position ^double angle] (car position (v/vec2 0.0 0.0) angle))
  ([position velocity ^double angle]
   (->Car position velocity angle (v/vec2 (m/cos angle) (m/sin angle)))))

(def basket-offset (v/vec2 30.0 0.0))

(defn baskets-position [^Car car]
  (let [rotated (v/rotate basket-offset (m/- (.angle car) m/HALF_PI))]
    [(v/add (.position car) rotated)
     (v/sub (.position car) rotated)]))

(defn step
  [^Car car ^double acc ^double rot]
  (let [^Vec2 pos (.position car)
        #_#_   drag (get-in n/grad-map [(unchecked-int (.x pos)) (unchecked-int (.y pos))])
        rot (m/* MAX-ROTATION (m/constrain rot -1.0 1.0))
        acc (let [a (m/constrain acc -1.0 1.0)]
              (if (m/neg? a)
                (m/* MAX-ACCELERATION-B a)
                (m/* MAX-ACCELERATION-F a)))
        npos (v/add pos (.velocity car))
        nvel (-> (.direction car)
                 (v/mult acc)
                 (v/add (.velocity car))
                 #_    (v/add drag)
                 (v/mult FRICTION)
                 (v/limit MAX-VELOCITY))
        nangle (m/mod (m/+ (.angle car) rot) m/TWO_PI)]
    (Car. npos nvel nangle (v/vec2 (m/cos nangle) (m/sin nangle)))))
