(ns examples.ex04-noise
  "Draw various noise variants"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (canvas 200 200 :low)) ;; low quality canvas (we draw pixel by pixel)

(defn draw-noise
  "Loop through noise field and draw it."
  [n]
  (with-canvas [canvas cnvs] ;; make graphical context
    (set-background canvas :black)
    (dotimes [y 180]
      (dotimes [x 180]
        (let [xx (/ x 30.0)
              yy (/ y 30.0)
              nn (* 255.0 ^double (n xx yy))]
          (set-color canvas (c/color nn nn nn))
          (rect canvas (+ x 10) (+ y 10) 1 1)))))
  canvas)

(show-window {:canvas cnvs
              :window-name "noise"
              :w 600
              :h 600
              :fps 10
              :hint :mid})

(defmethod key-pressed ["noise" \space] [_ _]
  (save canvas (next-filename "results/ex04/" ".jpg")))

(draw-noise r/noise)

;; [[../results/ex04/061F9A12_000000.jpg]]

(draw-noise r/vnoise)

;; [[../results/ex04/061F9A12_000001.jpg]]

(draw-noise r/simplex)

;; [[../results/ex04/061F9A12_000002.jpg]]

;; basis types
(keys r/noise-types)
;; => (:value :gradient :simplex)

;; interpolations
(keys r/interpolations)
;; => (:none :linear :hermite :quintic)

(defn draw-random-noise
  "Draw random noise for given fractal type"
  [t]
  (let [cfg {:interpolation (rand-nth (keys r/interpolations))
             :noise-type (rand-nth (keys r/noise-types))
             :octaves (r/irand 2 9)
             :gain (r/drand 0.25 0.75)}]
    (draw-noise (t cfg))
    cfg))

;; draw single octave
(draw-random-noise r/make-single-noise)

;; [[../results/ex04/061F9A12_000003.jpg]] [[../results/ex04/061F9A12_000004.jpg]] [[../results/ex04/061F9A12_000005.jpg]]

;; fractal brownian motion
(draw-random-noise r/make-fbm-noise)

;; [[../results/ex04/061F9A12_000006.jpg]] [[../results/ex04/061F9A12_000007.jpg]] [[../results/ex04/061F9A12_000008.jpg]]

;; billow blending
(draw-random-noise r/make-billow-noise)

;; [[../results/ex04/061F9A12_000009.jpg]] [[../results/ex04/061F9A12_000010.jpg]] [[../results/ex04/061F9A12_000011.jpg]]

;; ridgedmulti blending
(draw-random-noise r/make-ridgedmulti-noise)

;; [[../results/ex04/061F9A12_000012.jpg]] [[../results/ex04/061F9A12_000013.jpg]] [[../results/ex04/061F9A12_000014.jpg]]
