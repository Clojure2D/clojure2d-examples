;; variation of the http://www.iquilezles.org/www/articles/popcorns/popcorns.htm

(ns ex21-popcorn
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :as f])
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

(def sinusoidal (f/field :sinusoidal 1.0))

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
        (c2d/point canvas screenx screeny)
        (Vec2. nx ny))
      (make-particle))))

(defn get-noise
  ""
  [f ^Vec2 in]
  (let [^Vec2 in (v/mult in 0.3)]
    (Vec2. (- ^double (f (.x in) (.y in)) 0.5)
           (- ^double (f (.y in) (.x in) 0.3) 0.5))))

(binding [f/*skip-random-fields* true]
  (let [canvas (c2d/canvas w h)
        window (c2d/show-window canvas "popcorn")
        field-config (f/random-configuration)
        field (f/combine field-config)
        vrand (Vec2. (r/drand -1 1) (r/drand -1 1))
        noisef (r/randval 0.2 (partial get-noise (r/random-noise-fn)) (constantly (Vec2. 0.0 0.0)))
        mv-fun (partial move-particle vrand noisef field)
        
        particles (repeatedly 15000 make-particle)
        looper (fn [canvas] (loop [xs particles]
                             (if (c2d/window-active? window)
                               (recur (mapv (partial mv-fun canvas) xs))
                               canvas)))]
    
    (defmethod c2d/key-pressed ["popcorn" \space] [_ _]
      (binding [c2d/*jpeg-image-quality* 0.9]
        (c2d/save canvas (c2d/next-filename "results/ex21/" ".jpg"))))

    (println field-config)
    
    (c2d/with-canvas-> canvas
      (c2d/set-background 240 240 240)
      (c2d/set-color 49 52 59 alpha)
      (c2d/set-stroke point-size)
      (looper))))

