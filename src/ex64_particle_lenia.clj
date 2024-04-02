;; https://google-research.github.io/self-organising-systems/particle-lenia/
(ns ex64-particle-lenia
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defrecord Params [^double muk ^double sigmak ^double wk
                   ^double mug ^double sigmag ^double crep])

(defrecord Fields [^double U ^double G ^double R ^double E])

(defn peak-f
  ^double [^double x ^double mu ^double sigma]
  (m/exp (m/- (m/sq (m// (m/- x mu) sigma)))))

#_(defn peak-f2
    ^double [^double x ^double mu ^double sigma]
    (m/exp (m/- (m/abs (m// (m/- x mu) sigma)))))

(defn fields-f
  ^Fields [^Params p points ^Vec2 x]
  (let [dists (map (partial v/dist x) points)
        U (m/* (.wk p)
               (v/sum (map (fn [^double d] (peak-f d (.muk p) (.sigmak p))) dists)))
        G (peak-f U (.mug p) (.sigmag p))
        R (m/* 0.5 (.crep p) (v/sum (map (fn [^double d]
                                           (m/sq (m/max (m/- 1.0 d) 0.0))) dists)))]
    (Fields. U G R (m/- R G))))

(defn gradient [f]
  (fn ^Vec2 [^Vec2 v]
    (let [^double x1 (f (Vec2. (m/+ (.x v) 1.0e-6) (.y v)))
          ^double x2 (f (Vec2. (m/- (.x v) 1.0e-6) (.y v)))
          ^double y1 (f (Vec2. (.x v) (m/+ (.y v) 1.0e-6)))
          ^double y2 (f (Vec2. (.x v) (m/- (.y v) 1.0e-6)))]
      (Vec2. (m/* (m/- x1 x2) 500000.0)
             (m/* (m/- y1 y2) 500000.0)))))

(defn motion-f
  [p points]
  (let [grad-f (gradient (fn [x] (let [^Fields f (fields-f p points x)] (.E f))))]
    (pmap grad-f points)))

(defn make-step
  [p points]
  (let [ds (motion-f p points)]
    (map (fn [p d]
           (v/sub p (v/mult d 0.1))) points ds)))

(def params (map->Params {:muk 4.0 :sigmak 1.0 :wk 0.022 :mug 0.6 :sigmag 0.15 :crep 1.0}))

(def ^:const scaling 18)
(def ^:const csize 800)

(defn draw
  [canvas _ _ points]
  (c2d/set-background canvas (c/color 10 10 20) 100)
  (c2d/translate canvas (m// csize 2) (m// csize 2))
  (c2d/set-color canvas (c/color 240 240 242 230))
  (doseq [^Vec2 p points 
          :let [^Vec2 p (v/mult p scaling)]]
    (c2d/ellipse canvas (.x p) (.y p) 6 6))
  (nth (iterate (partial make-step params) points) 4))

(def window (c2d/show-window {:canvas (c2d/black-canvas csize csize)
                            :draw-fn draw
                            :background :black
                            :draw-state (repeatedly 350 #(Vec2. (r/grand 3.5)
                                                                (r/grand 3.5)))}))
