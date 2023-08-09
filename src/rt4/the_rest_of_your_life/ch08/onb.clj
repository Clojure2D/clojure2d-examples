(ns rt4.the-rest-of-your-life.ch08.onb
  (:require [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol ONBProto
  (local [onb a b c] [onb a]))

(defrecord ONB [^Vec3 u ^Vec3 v ^Vec3 w]
  ONBProto
  (local [_ a b c] (-> (v/mult u a)
                       (v/add (v/mult v b))
                       (v/add (v/mult w c))))
  (local [onb a] (let [^Vec3 a a]
                   (local onb (.x a) (.y a) (.z a)))))

(def v010 (Vec3. 0.0 1.0 0.0))
(def v100 (Vec3. 1.0 0.0 0.0))

(defn build-from-w
  ^ONB [^Vec3 w]
  (let [^Vec3 unit-w (v/normalize w)
        a (if (> (m/abs (.x unit-w)) 0.9)
            v010 v100)
        v (v/normalize (v/cross unit-w a))
        u (v/cross unit-w v)]
    (ONB. u v unit-w)))
