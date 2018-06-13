;; variation of the http://www.iquilezles.org/www/articles/popcorns/popcorns.htm

(ns ex21-popcorn
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :refer :all])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 900)
(def ^:const ^long h 900)

(def ^:const ^double point-step 0.02) ; 0.01 - 2.0
(def ^:const ^double point-size 1.2) ; 0.6 - 1.2
(def ^:const ^int alpha 20)

(defn make-particle
  ""
  []
  (let [r (r/drand 0.5 m/TWO_PI)
        a (r/drand m/TWO_PI)]
    (Vec2. (* r (m/qcos a)) (* r (m/qsin a)))))

(def sinusoidal (field :sinusoidal 1.0))

(defn move-particle
  ""
  [^Vec2 vrand noisef fun canvas ^Vec2 in]
  (let [^Vec2 nf (noisef in)
        ^Vec2 v (v/add in (v/mult (sinusoidal (v/mult (->> in
                                                           (v/add vrand)
                                                           fun
                                                           (v/add nf)) m/TWO_PI)) point-step))
        nx (.x v)
        ny (.y v)
        screenx (m/norm nx -8.0 8.0 0 w)
        screeny (m/norm ny -8.0 8.0 0 h)]
    (if (and (<= 40 screeny (- h 41)) (<= 40 screenx (- w 41)))
      (do
        (point canvas screenx screeny)
        (Vec2. nx ny))
      (make-particle))))

(defn get-noise
  ""
  [f ^Vec2 in]
  (let [^Vec2 in (v/mult in 0.3)]
    (Vec2. (- ^double (f (.x in) (.y in)) 0.5)
           (- ^double (f (.y in) (.x in) 0.3) 0.5))))

(binding [*skip-random-fields* true]
  (let [canvas (canvas w h)
        window (show-window canvas "popcorn")
        field-config (random-configuration)
        field (combine field-config)
        vrand (Vec2. (r/drand -1 1) (r/drand -1 1))
        noisef (r/randval 0.2 (partial get-noise (r/random-noise-fn)) (constantly (Vec2. 0.0 0.0)))
        mv-fun (partial move-particle vrand noisef field)
        
        particles (repeatedly 15000 make-particle)
        looper (fn [canvas] (loop [xs particles]
                              (if (window-active? window)
                                (recur (mapv (partial mv-fun canvas) xs))
                                canvas)))]
    
    (defmethod key-pressed ["popcorn" \space] [_ _]
      (binding [*jpeg-image-quality* 0.9]
        (save canvas (next-filename "results/ex21/" ".jpg"))))

    (println field-config)
    
    (with-canvas-> canvas
      (set-background 240 240 240)
      (set-color 49 52 59 alpha)
      (set-stroke point-size)
      (looper))))

