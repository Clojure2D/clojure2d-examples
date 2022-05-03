(ns rt4.in-one-weekend.ch08c.interval
  (:refer-clojure :exclude [empty])
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol IntervalProto
  (contains [interval x]) ;; a <= x <= b
  ;; introduced due to the bug in the book (in the time of writing this code), a < x <= b
  (contains- [interval x])
  (clamp [interval x])) 

(defrecord Interval [^double mn ^double mx]
  IntervalProto
  (contains [_ x] (m/between? mn mx ^double x))
  (contains- [_ x] (m/between-? mn mx ^double x))
  (clamp [_ x] (m/constrain ^double x mn mx)))

(defn interval
  ([] (->Interval ##Inf ##-Inf))
  ([m] (map->Interval m))
  ([^double mn ^double mx] (->Interval mn mx)))

(def empty (interval))
(def universe (interval ##-Inf ##Inf))
