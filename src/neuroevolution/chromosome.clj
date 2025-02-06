(ns neuroevolution.chromosome
  (:require [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn gen-perceptron [^long in] (conj (repeatedly (m/inc in) r/grand)))
(defn gen-layer [[in out]] (reduce concat (repeatedly out (partial gen-perceptron in))))
(defn random-net [profile] (double-array (mapcat gen-layer (partition 2 1 profile))))

(defn mutate [^double probability ^doubles chromosome]
  (let [cnt (alength chromosome)
        mx (unchecked-int (m/inc (m/* cnt probability)))
        ^doubles nchr (double-array cnt)]
    (System/arraycopy chromosome 0 nchr 0 cnt)
    (dotimes [_ (r/irand mx)]
      (let [pos (r/irand cnt)]
        (Array/aset nchr pos (case (unchecked-int (r/irand 10))
                               0 (m/* 2.0 (Array/aget chromosome pos))
                               1 (m/- (Array/aget chromosome pos))
                               2 (m/* 0.5 (Array/aget chromosome pos))
                               3 (r/grand 2)
                               4 (r/grand 10)
                               5 (r/grand 100)
                               6 (m/* 1.1 (Array/aget chromosome pos))
                               7 (m/* 0.9 (Array/aget chromosome pos))
                               (r/grand)))))
    nchr))

(defn crossover
  ([[chromosome1 chromosome2]] (crossover chromosome1 chromosome2))
  ([^doubles chromosome1 ^doubles chromosome2]
   (let [cnt (alength chromosome1)
         p (r/irand 1 cnt)
         p- (m/- cnt p)
         t1 (double-array cnt)
         t2 (double-array cnt)]
     (System/arraycopy chromosome1 0 t1 0 p)
     (System/arraycopy chromosome2 p t1 p p-)
     (System/arraycopy chromosome2 0 t2 0 p)
     (System/arraycopy chromosome1 p t2 p p-)
     [t1 t2])))


