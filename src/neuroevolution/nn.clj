(ns neuroevolution.nn
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn elu ^double [^double x] (if (m/neg? x) (m/dec (m/exp x)) x))
(defn lrelu ^double [^double x] (if (m/neg? x) (m/* 0.01 x) x))
(defn selu ^double [^double x] (m/* 1.0507 (if (m/neg? x) (m/* 1.67326 (m/dec (m/exp x))) x)))
(defn swish ^double [^double x] (m/* x (m/sigmoid x)))
(defn elish ^double [^double x] (m/* (elu x) (m/sigmoid x)))
(defn softsign ^double [^double x] (m// x (m/inc (m/abs x))))


(defn evaluate
  ([profile ^doubles chromosome in] (evaluate profile chromosome 0 in))
  ([profile ^doubles chromosome ^long layer-pos in]
   (if-not (seq profile)
     in
     (let [lc (long (profile 0))
           cnt (count in)]
       (loop [pos (long 0)
              curr-pos layer-pos
              out []]
         (if (m/== pos lc)
           (evaluate (subvec profile 1) chromosome curr-pos (double-array (conj out 1.0)))
           (let [next-pos (m/+ curr-pos cnt)
                 wsb (java.util.Arrays/copyOfRange chromosome curr-pos next-pos)]
             (recur (m/inc pos)
                    next-pos
                    (conj out (softsign (v/dot in wsb)))))))))))

