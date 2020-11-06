(ns ex05-particles
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.fields :as f]
            [clojure.pprint :refer [pprint]])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 1200) ;; canvas size
(def ^:const ^long h 1200)

;; configuration
(def ^:const ^double point-step 15.0) ;; step for next point (15 pixels)
(def ^:const ^double rscale 25.0) ;; vector field scale down factor
(def ^:const ^double angle-mult 16.0) ;; how much multiply angle taken from noise
(def ^:const ^double point-size 0.9) ;; size of point
(def ^:const ^int alpha 10) ;; alpha for point

(defn make-particle
  "Create random particle"
  []
  (Vec2. (r/drand w) (r/drand h)))

(defn move-particle
  "Move particle,

  vrand - random vector field shift
  line? - draw line or point
  fun - vector field
  noise - scalar field
  canvas - where to draw
  in - current particle posistion"
  [^Vec2 vrand line? fun noise canvas ^Vec2 in]
  (let [xx (m/norm (.x in) 0 w -2 2) ;; normalize particle
        yy (m/norm (.y in) 0 h -2 2)
        ^Vec2 vr (v/add vrand (Vec2. xx yy)) ;; shift position
        ^Vec2 v (v/div (fun vr) rscale) ;; take vector field value and scale down
        ^double n (noise (.x v) (.y v)) ;; take scalar value from noise and...
        ang (* n m/TWO_PI angle-mult) ;; ...treat as direction
        nx (+ (.x in) (* point-step (m/qcos ang))) ;; next position
        ny (+ (.y in) (* point-step (m/qsin ang))) 
        col (m/cnorm (m/sqrt n) 0 1 100 240)] ;; particle color
    
    (if (and (<= 80 ny (- h 81)) (<= 80 nx (- w 81))) ;; if inside window, draw. Create new particle otherwise.
      (do
        (c2d/set-color canvas col col col alpha)
        
        (if line?
          (c2d/line canvas (.x in) (.y in) nx ny)
          (c2d/point canvas nx ny))
        
        (Vec2. nx ny))
      (make-particle))))

(binding [f/*skip-random-fields* true]
  (let [cnvs (c2d/canvas w h) ;; canvas
        window (c2d/show-window cnvs "particles" w h 25) ;; window
        noise (r/random-noise-fn) ;; scalar field (random noise)
        field-config (f/random-configuration) ;; vector field random configuration
        field (f/combine field-config) ;; vector field
        vrand (Vec2. (r/drand -1 1) (r/drand -1 1)) ;; vector field shift
        mv-fun (partial move-particle vrand (r/brand) field noise)  ;; move particle function with current config
        particles (repeatedly 25000 make-particle) ;; particle list
        looper (fn [cnvs] (loop [xs particles] ;; loop and draw unless window is visible (close window to stop)
                           (if (c2d/window-active? window)
                             (recur (mapv (partial mv-fun cnvs) xs))
                             cnvs)))]
    
    (defmethod c2d/key-pressed ["particles" \space] [_ _]
      (c2d/save cnvs (c2d/next-filename "results/ex05/" ".jpg")))

    (pprint field-config)

    (c2d/with-canvas-> cnvs
      (c2d/set-background 10 10 10)
      (c2d/set-stroke point-size)
      looper))) ;; run!

