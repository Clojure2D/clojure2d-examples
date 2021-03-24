;; Double pendulum simulation using sicmutils
;;
;; https://github.com/sicmutils/sicmutils/blob/master/src/sicmutils/examples/double_pendulum.cljc
;; https://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics-0
(ns ex28-double-pendulum 
  (:require [sicmutils.examples.double-pendulum :as dp]
            ;; [sicmutils.structure :as ss]
            [clojure2d.core :as c2d] 
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; define canvas, window
(def cnvs (c2d/canvas 600 600 :low))
(def window (c2d/show-window cnvs "Double pendulum"))

;; canvas is refreshed externally by integrator, let's define frame rate
(def ^:const ^double time-delay (/ 1000.0 ^double (:fps window)))

;; pendulum settings
(def ^:const ^double len 0.6) ; length of first rod, second has length (- 1.0 len)
(def ^:const ^double mass1 4) ; mass of the first ball
(def ^:const ^double mass2 10) ; mass of the second ball
(def ^:const ^double theta (- m/PI 0.1)) ; angle of the first rod
(def ^:const ^double phi (r/drand m/TWO_PI)) ; angle of the second rod
(def ^:const ^double simulation-time 10.0) ; time of simulation (it's not animation time)
(def ^:const ^double speed 0.5) ; simulation speed, set to 1.0 to have real time animation

(do
  ;; precalculated parameters
  (def ^:const ^double len1 len)
  (def ^:const ^double len2 (- 1.0 len))

  (def ^:const ^double l1 (* len1 250.0))
  (def ^:const ^double l2 (* len2 250.0))

  (def ^:const ^double m1 (* 20.0 (m/cbrt mass1)))
  (def ^:const ^double m2 (* 20.0 (m/cbrt mass2)))

  ;; local drawing buffer
  (def local-canvas (c2d/canvas 600 600))

  (defn observe
    "StepHandler callback function (see org.apache.commons.math3.ode.nonstiff.GraggBulirschStoerIntegrator)"
    [_ state]
    (let [[_ [a0 a1] _] state ;; current position (angles)
          ;; polar to cartesian
          posx1 (+ 300.0 (* l1 (m/sin a0)))
          posy1 (+ 300.0 (* l1 (m/cos a0)))
          posx2 (+ posx1 (* l2 (m/sin a1)))
          posy2 (+ posy1 (* l2 (m/cos a1)))]

      ;; draw rods, balls on buffer
      (c2d/with-canvas-> local-canvas
        (c2d/set-color 0 0 0 220)
        (c2d/rect 0 0 600 600)
        (c2d/set-color 250 250 250)
        (c2d/line 300 300 posx1 posy1)
        (c2d/line posx1 posy1 posx2 posy2)
        (c2d/set-color 0 0 255)
        (c2d/rect 295 295 10 10)
        (c2d/set-color 255 0 0)
        (c2d/ellipse posx1 posy1 m1 m1)
        (c2d/ellipse posx2 posy2 m2 m2))

      ;; draw on window
      (c2d/with-canvas-> cnvs
        (c2d/image (c2d/get-image local-canvas)))

      ;; wait
      (Thread/sleep time-delay)))

  ;; run integration
  (future (dp/evolver {:t simulation-time
                       :dt (* speed (/ 1.0 ^double (:fps window)))
                       :l1 len1
                       :l2 len2
                       :m1 mass1
                       :m2 mass2
                       :theta_0 theta
                       :phi_0 phi
                       :observe observe})))

(defmethod c2d/key-pressed ["Double pendulum" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex28/" ".jpg")))
