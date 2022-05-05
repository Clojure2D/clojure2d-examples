(ns rt4.the-rest-of-your-life.ch06b.perlin
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Perlin [ranvec perm-x perm-y perm-z])

(defn- generate-perm []
  (vec (shuffle (range 256))))

(defn perlin
  []
  (->Perlin (vec (repeatedly 256 (fn [] (v/normalize (v/generate-vec3 #(r/drand -1.0 1.0))))))
            (generate-perm) (generate-perm) (generate-perm)))

(def ^:private corners [0 1])

(defn trilinear-interp
  ^double [c ^double u ^double v ^double w]
  (let [u1 (- 1.0 u)
        v1 (- 1.0 v)
        w1 (- 1.0 w)]
    (reduce m/fast+ (map m/fast*
                         c (for [^long i corners ^long j corners ^long k corners]
                             (* (+ (* i u) (* (- 1.0 i) u1))
                                (+ (* j v) (* (- 1.0 j) v1))
                                (+ (* k w) (* (- 1.0 k) w1))))))))

(defn perlin-interp
  ^double [c ^double u ^double v ^double w]
  (let [uu (* u u (- 3.0 (* 2.0 u)))
        vv (* v v (- 3.0 (* 2.0 v)))
        ww (* w w (- 3.0 (* 2.0 w)))
        uu1 (- 1.0 uu)
        vv1 (- 1.0 vv)
        ww1 (- 1.0 ww)
        cs (->> (for [^long i corners ^long j corners ^long k corners]
                  (v/vec3 (- u i) (- v j) (- w k)))
                (map v/dot c))]
    (->> (for [^long i corners ^long j corners ^long k corners]
           (* (+ (* i uu) (* (- 1.0 i) uu1))
              (+ (* j vv) (* (- 1.0 j) vv1))
              (+ (* k ww) (* (- 1.0 k) ww1))))
         (map m/fast* cs)
         (reduce m/fast+))))

(defn noise
  ^double [^Perlin perlin ^Vec3 p]
  (let [i (long (m/floor (.x p)))
        j (long (m/floor (.y p)))
        k (long (m/floor (.z p)))
        u (- (.x p) i)
        v (- (.y p) j)
        w (- (.z p) k)
        ]
    (-> (for [^long di corners ^long dj corners ^long dk corners]
          ((.ranvec perlin)
           (bit-xor ((.perm-x perlin) (bit-and (+ i di) 0xff) )
                    ((.perm-y perlin) (bit-and (+ j dj) 0xff))
                    ((.perm-z perlin) (bit-and (+ k dk) 0xff)))))
        (perlin-interp u v w))))

(defn turb
  (^double [^Perlin perlin ^Vec3 p] (turb perlin p 7))
  (^double [^Perlin perlin ^Vec3 p ^long depth]
   (loop [i (long 0)
          accum 0.0
          temp-p p
          weight 1.0]
     (if (< i depth)
       (recur (inc i)
              (+ accum (* weight (noise perlin temp-p)))
              (v/mult temp-p 2.0)
              (* 0.5 weight))
       (m/abs accum)))))
