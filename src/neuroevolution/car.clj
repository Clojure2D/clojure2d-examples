(ns neuroevolution.car
  (:require [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defrecord CarConfig [^double max-velocity ^double max-acceleration-f ^double max-acceleration-b
                      ^double max-rotation ^double friction-factor])

(def default-config {:max-velocity 15.0
                   :max-acceleration-f 1.5
                   :max-acceleration-b 0.7
                   :max-rotation 0.05
                   :friction-factor 0.95})

(defrecord Car [position velocity ^double angle direction ^CarConfig config])

(defn car
  ([position] (car position (r/drand m/TWO_PI)))
  ([position ^double angle] (car position (v/vec2 0.0 0.0) angle))
  ([position velocity ^double angle] (car position velocity angle {}))
  ([position velocity ^double angle config]
   (->Car position
          velocity
          angle
          (v/normalize (v/vec2 (m/cos angle) (m/sin angle)))
          (map->CarConfig (merge default-config config)))))

(def basket-offset (v/vec2 30.0 0.0))

(defn baskets-position [^Car car]
  (let [rotated (v/rotate basket-offset (m/- (.angle car) m/HALF_PI))]
    [(v/add (.position car) rotated)
     (v/sub (.position car) rotated)]))

(defn step
  [^Car car ^double acc ^double rot]
  (let [^CarConfig config (.config car)
        ^Vec2 pos (.position car)        
        rot (m/* (.max-rotation config) (m/constrain rot -1.0 1.0))
        acc (let [a (m/constrain acc -1.0 1.0)]
              (if (m/neg? a)
                (m/* (.max-acceleration-b config) a)
                (m/* (.max-acceleration-f config) a)))
        npos (v/add pos (.velocity car))
        nvel (-> (.direction car)
                 (v/mult acc)
                 (v/add (.velocity car))
                 (v/mult (.friction-factor config))
                 (v/limit (.max-velocity config)))
        nangle (m/mod (m/+ (.angle car) rot) m/TWO_PI)]
    (Car. npos nvel nangle (v/vec2 (m/cos nangle) (m/sin nangle)) config)))
