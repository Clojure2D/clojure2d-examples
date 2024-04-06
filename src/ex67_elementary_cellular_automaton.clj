(ns ex67-elementary-cellular-automaton
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as utils]))

(defn init-random! [buff]
  (doseq [x (range (c2d/width buff))]
    (p/set-color! buff x 0 (r/randval 0.6 :black :white))))

(defn init-one! [buff]
  (p/set-color! buff (/ (c2d/width buff) 2) 0 :white))

;; 3 bits

(defn rule3 [^long n ^long x- ^long x ^long x+]
  (let [v (+ (if (pos? x-) 4 0)
             (if (pos? x) 2 0)
             (if (pos? x+) 1 0))]
    (if (bit-test n v) :white :black)))

(defn apply-rule3! [buff ^long n]
  (let [w (c2d/width buff)]
    (doseq [y (range 1 (c2d/height buff))
            x (range w)
            :let [y- (dec y)
                  x- (mod (dec x) w)
                  x+ (mod (inc x) w)]]
      (p/set-color! buff x y (rule3 n
                                    (p/get-value buff 0 x- y-)
                                    (p/get-value buff 0 x y-)
                                    (p/get-value buff 0 x+ y-))))))

(defn show-rule3
  ([] (let [r (r/lrand 256)]
        (println "rule =" r)
        (show-rule3 r false)))
  ([n] (show-rule3 n true))
  ([n one?]
   (let [buff (p/pixels 300 400)]
     (if one?
       (init-one! buff)
       (init-random! buff))
     (apply-rule3! buff n)
     (utils/show-image (c2d/resize buff 600 800)))))

;; 30, 105, 129, 82, 60, 73, 99
#_(show-rule3 99)

;;;;;

(defn rule5 [^long n [^long x-- ^long x- ^long x ^long x+ ^long x++]]
  (let [v (+ (if (pos? x--) 16 0)
             (if (pos? x-) 8 0)
             (if (pos? x) 4 0)
             (if (pos? x+) 2 0)
             (if (pos? x++) 1 0))]
    (if (bit-test n v) :white :black)))

(defn apply-rule5! [buff ^long n]
  (let [w (c2d/width buff)]
    (doseq [y (range 1 (c2d/height buff))
            x (range w)
            :let [y- (dec y)
                  x-- (mod (- x 2) w)
                  x- (mod (dec x) w)
                  x+ (mod (inc x) w)
                  x++ (mod (+ x 2) w)]]
      (p/set-color! buff x y (rule5 n [(p/get-value buff 0 x-- y-)
                                       (p/get-value buff 0 x- y-)
                                       (p/get-value buff 0 x y-)
                                       (p/get-value buff 0 x+ y-)
                                       (p/get-value buff 0 x++ y-)])))))

(defn show-rule5
  ([] (let [r (bit-and (r/lrand) 0xffffffff)]
        (println "rule =" r)
        (show-rule5 r true)))
  ([n] (show-rule5 n true))
  ([n one?]
   (let [buff (p/pixels 300 400)]
     (if one?
       (init-one! buff)
       (init-random! buff))
     (apply-rule5! buff n)
     (utils/show-image (c2d/resize buff 600 800)))))

#_(show-rule5 (r/lrand) false)
