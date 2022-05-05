(ns rt4.the-rest-of-your-life.ch02a.pi
  (:require [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

m/PI
;; => 3.141592653589793

(defn listing-2
  ([] (listing-2 100000))
  ([^long iters] (/ (* 4.0 ^long (listing-2 iters 0)) iters))
  ([^long iters ^long accum]
   (if (zero? iters)
     accum
     (recur (dec iters) (let [x (r/drand -1.0 1.0)
                              y (r/drand -1.0 1.0)]
                          (if (< (+ (* x x) (* y y)) 1.0)
                            (inc accum) accum))))))


(repeatedly 10 listing-2)
;; => (3.15384 3.13576 3.14896 3.1328 3.1428 3.1436 3.14048 3.14324 3.14976 3.14972)

;;;;

(defn listing-3-iter
  ^long [^long accum]
  (let [x (r/drand -1.0 1.0)
        y (r/drand -1.0 1.0)]
    (if (< (+ (* x x) (* y y)) 1.0)
      (inc accum) accum)))

(defn listing-3
  ([] (listing-3 100000))
  ([^long step]
   (map-indexed (fn [^long id ^long inside-circle]
                  (/ (* 4.0 inside-circle)
                     (* step (inc id)))) (rest (take-nth step (rest (iterate listing-3-iter 0)))))))


(take 20 (listing-3))
;; => (3.12948 3.13724 3.13696 3.13978 3.142352 3.1431066666666667 3.142594285714286 3.14311 3.1428755555555554 3.1427 3.142450909090909 3.14202 3.1412184615384615 3.1412085714285714 3.14108 3.1409975 3.1404635294117647 3.1400888888888887 3.140254736842105 3.140258)

;;;;

(defn listing-4
  ([] (listing-4 100000))
  ([^long iters] (let [sqrt-iters (long (m/sqrt iters))
                       sqrt-iters- (dec sqrt-iters)
                       calc-fn (fn ^long [^long i ^long j ^long accum]
                                 (if (neg? i)
                                   accum
                                   (if (neg? j)
                                     (recur (dec i) sqrt-iters- accum)
                                     (recur i (dec j) (let [x (dec (* 2.0 (/ (+ i (r/drand)) sqrt-iters)))
                                                            y (dec (* 2.0 (/ (+ j (r/drand)) sqrt-iters)))]
                                                        (if (< (+ (* x x) (* y y)) 1.0)
                                                          (inc accum) accum))))))]
                   (/ (* 4.0 ^long (calc-fn sqrt-iters- sqrt-iters- 0))
                      (* sqrt-iters sqrt-iters)))))

{:regular (listing-2)
 :stratified (listing-4)}
;; => {:regular 3.14372, :stratified 3.142004486460503}
