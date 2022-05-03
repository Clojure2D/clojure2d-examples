(ns rt4.the-next-week.ch05c.perlin
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

(def ^:private corners [0 1])

(defn- trilinear-interp
  [c ^double u ^double v ^double w]
  (let [u1 (- 1.0 u)
        v1 (- 1.0 v)
        w1 (- 1.0 w)]
    (reduce m/fast+ (map m/fast*
                         c (for [^long i corners ^long j corners ^long k corners]
                             (* (+ (* i u) (* (- 1.0 i) u1))
                                (+ (* j v) (* (- 1.0 j) v1))
                                (+ (* k w) (* (- 1.0 k) w1))))))))

(defn noise
  [^Perlin perlin ^Vec3 p]
  (let [i (long (m/floor (.x p)))
        j (long (m/floor (.y p)))
        k (long (m/floor (.z p)))
        u (- (.x p) i)
        v (- (.y p) j)
        w (- (.z p) k)
        u (* u u (- 3.0 (* 2.0 u)))
        v (* v v (- 3.0 (* 2.0 v)))
        w (* w w (- 3.0 (* 2.0 w)))]
    (-> (for [^long di corners ^long dj corners ^long dk corners]
          ((.ranfloat perlin)
           (bit-xor ((.perm-x perlin) (bit-and (+ i di) 0xff) )
                    ((.perm-y perlin) (bit-and (+ j dj) 0xff))
                    ((.perm-z perlin) (bit-and (+ k dk) 0xff)))))
        (trilinear-interp u v w))))
