(ns rt4.the-rest-of-your-life.ch04.sphere-importance
  (:require [fastmath.core :as m]            
            [fastmath.vector :as v]
            [rt4.common :refer [random-unit-vector]])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn listing-16
  ([{:keys [f pdf iters]
     :or {iters 1000000}}] (listing-16 f pdf iters))
  ([f pdf ^long iters]
   (let [iter (fn [^long iters ^double accum]
                (if (zero? iters)
                  accum
                  (recur (dec iters) (let [d (random-unit-vector)
                                           ^double f-d (f d)]
                                       (+ accum (/ f-d ^double (pdf d)))))))]
     (/ ^double (iter iters 0.0) iters))))

(* m/PI (/ 4 3.0))
;; => 4.1887902047863905

(listing-16 {:f (fn [^Vec3 d] (* (.z d) (.z d)))
             :pdf (constantly (/ (* 2.0 m/TWO_PI)))})
;; => 4.1844322859715675

