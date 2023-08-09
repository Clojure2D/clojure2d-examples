(ns rt4.the-rest-of-your-life.ch03.integrate
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.stats :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; integrating x^2

(/ 8.0 3.0)
;; => 2.6666666666666665

(defn listing-8
  ([f] (listing-8 f 1000000 0.0 2.0))
  ([f ^long iters ^double a ^double b]
   (let [iter (fn [^long iters ^double accum]
                (if (zero? iters)
                  accum
                  (recur (dec iters) (let [x (r/drand a b)]
                                       (+ accum ^double (f x))))))]
     (* (- b a) (/ ^double (iter iters 0.0) iters)))))

(listing-8 (fn [^double x] (* x x)))
;; => 2.6681514406724016

(listing-8 (fn [^double x] (m/pow (m/sin x) 5.0)))
;; => 0.9033734864212455

(listing-8 (fn [^double x] (m/log (m/sin x))))
;; => -1.1039922931576298


;; estimate half-way

(defn gen-x-px []
  (let [x (r/drand m/TWO_PI)
        sin-x (m/sin x)
        p-x (* (m/exp (/ x m/-TWO_PI))
               sin-x sin-x)]
    [x p-x]))

(defn listing-12
  []
  (let [N 10000
        samples (->> (repeatedly N gen-x-px)
                     (sort-by first))
        sum (stats/sum (map second samples))
        half-sum (/ sum 2.0)
        halfway-point (reduce (fn [^double accum [^double x ^double p-x]]
                                (if (>= accum half-sum)
                                  (reduced x)
                                  (+ accum p-x))) 0.0 samples)]
    {:average (/ sum N)
     :area-under-curve (* m/TWO_PI (/ sum N))
     :halfway-point halfway-point}))

(listing-12)
;; => {:average 0.30948931327086354,
;;     :area-under-curve 1.9445787058725899,
;;     :halfway-point 2.059948270921272}

;;

(defn listing-13
  ([{:keys [f pdf iters]
     :or {iters 1000000}}] (listing-13 f pdf iters))
  ([f pdf ^long iters]
   (let [iter (fn [^long iters ^double accum]
                (if (zero? iters)
                  accum
                  (recur (dec iters) (let [^double x (f (r/drand))]
                                       (+ accum (/ (* x x) ^double (pdf x)))))))]
     (/ ^double (iter iters 0.0) iters))))

;; uniform
(listing-13 {:f (fn [^double d] (* 2.0 d))
             :pdf (constantly 0.5)})
;; => 2.6680893521123763


;; linear
(listing-13 {:f (fn [^double d] (m/sqrt (* 4.0 d)))
             :pdf (fn [^double x] (/ x 2.0))})
;; => 2.667072983732895

;; final
(listing-13 {:f (fn [^double d] (* 8.0 (m/pow d m/THIRD)))
             :pdf (fn [^double x] (* (/ 3.0 8.0)
                                    (* x x)))
             :iters 1})
;; => 2.666666666666667
