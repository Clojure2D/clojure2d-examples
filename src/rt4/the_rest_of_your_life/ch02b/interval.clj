(ns rt4.the-rest-of-your-life.ch02b.interval
  (:refer-clojure :exclude [empty size])
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol IntervalProto
  (contains [interval x]) ;; a <= x <= b
  ;; introduced due to the bug in the book (in the time of writing this code), a < x <= b
  (contains- [interval x])
  (clamp [interval x])
  (expand [interval delta])) 

(defrecord Interval [^double mn ^double mx]
  IntervalProto
  (contains [_ x] (m/between? mn mx ^double x))
  (contains- [_ x] (m/between-? mn mx ^double x))
  (clamp [_ x] (m/constrain ^double x mn mx))
  (expand [_ delta]
    (let [padding (/ ^double delta 2.0)]
      (->Interval (- mn padding) (+ mn padding)))))

(defn interval
  ([] (->Interval ##Inf ##-Inf))
  ([m] (map->Interval m))
  ([^double mn ^double mx] (->Interval mn mx)))

(def empty (interval))
(def universe (interval ##-Inf ##Inf))

(defn merge-intervals
  [^Interval int0 ^Interval int1]
  (->Interval (min (.mn int0) (.mn int1)) (max (.mx int0) (.mx int1))))

(defn shift
  [^Interval int ^double displacement]
  (->Interval (+ (.mn int) displacement)
              (+ (.mx int) displacement)))

(defn size
  ^double [^Interval in]
  (- (.mx in) (.mn in)))
