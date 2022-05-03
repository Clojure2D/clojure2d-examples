(ns rt4.the-next-week.ch05a.perlin
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Perlin [ranfloat perm-x perm-y perm-z])

(defn- generate-perm []
  (vec (shuffle (range 256))))

(defn perlin
  []
  (->Perlin (vec (repeatedly 256 r/drand))
            (generate-perm) (generate-perm) (generate-perm)))

(defn noise
  [^Perlin perlin ^Vec3 p]
  (let [i (bit-and (int (* 4.0 (.x p))) 0xff)
        j (bit-and (int (* 4.0 (.y p))) 0xff)
        k (bit-and (int (* 4.0 (.z p))) 0xff)]
    ((.ranfloat perlin)
     (bit-xor ((.perm-x perlin) i)
              ((.perm-y perlin) j)
              ((.perm-z perlin) k)))))
