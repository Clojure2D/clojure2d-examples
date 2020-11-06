(ns ex04-noise
  "Draw various noise variants"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 200 200 :low)) ;; low quality canvas (we draw pixel by pixel)

(defn draw-noise
  "Loop through noise field and draw it."
  [n]
  (c2d/with-canvas [canvas cnvs] ;; make graphical context
    (c2d/set-background canvas :black)
    (dotimes [y 180]
      (dotimes [x 180]
        (let [xx (/ x 30.0)
              yy (/ y 30.0)
              nn (* 255.0 ^double (n xx yy))]
          (c2d/set-color canvas (c/color nn nn nn))
          (c2d/rect canvas (+ x 10) (+ y 10) 1 1))))
    canvas))

(c2d/show-window {:canvas cnvs
                  :window-name "noise"
                  :w 600
                  :h 600
                  :fps 10
                  :hint :mid})

(defmethod c2d/key-pressed ["noise" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex04/" ".jpg")))

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
(keys r/noise-interpolations)
;; => (:none :linear :hermite :quintic)

(defn draw-random-noise
  "Draw random noise for given fractal type"
  [t]
  (let [cfg {:interpolation (rand-nth (keys r/noise-interpolations))
             :noise-type (rand-nth (keys r/noise-types))
             :octaves (r/irand 2 9)
             :gain (r/drand 0.25 0.75)}]
    (draw-noise (t cfg))
    cfg))

;; draw single octave
(draw-random-noise r/single-noise)

;; [[../results/ex04/061F9A12_000003.jpg]] [[../results/ex04/061F9A12_000004.jpg]] [[../results/ex04/061F9A12_000005.jpg]]

;; fractal brownian motion
(draw-random-noise r/fbm-noise)

;; [[../results/ex04/061F9A12_000006.jpg]] [[../results/ex04/061F9A12_000007.jpg]] [[../results/ex04/061F9A12_000008.jpg]]

;; billow blending
(draw-random-noise r/billow-noise)

;; [[../results/ex04/061F9A12_000009.jpg]] [[../results/ex04/061F9A12_000010.jpg]] [[../results/ex04/061F9A12_000011.jpg]]

;; ridgedmulti blending
(draw-random-noise r/ridgedmulti-noise)

;; [[../results/ex04/061F9A12_000012.jpg]] [[../results/ex04/061F9A12_000013.jpg]] [[../results/ex04/061F9A12_000014.jpg]]

(defn draw-random-warp
  "Draw random warped noise."
  []
  (let [cfg (r/random-noise-cfg {:warp-scale (r/drand 0.1 10)})]
    (draw-noise (r/random-noise-fn cfg))
    cfg))

(draw-random-warp)

;; [[../results/ex04/6A2E760C_000006.jpg]] [[../results/ex04/6A2E760C_000004.jpg]] [[../results/ex04/6A2E760C_000005.jpg]]
