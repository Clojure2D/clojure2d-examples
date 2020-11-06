;; apply analog filter on image

(ns ex16-analog
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.signal :as s]
            [clojure2d.extra.signal :as sig])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; load image
(def ^Pixels p (p/load-pixels "results/test.jpg"))

(def cnvs (c2d/canvas (c2d/width p) (c2d/height p)))
(def window (c2d/show-window cnvs "Signal" 15 nil))

(defmethod c2d/key-pressed ["Signal" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex16/" ".jpg")))

;; dj-eq
(def effect1 (s/effect :dj-eq {:lo -10 :mid 10 :hi -10 :peak-bw 1.3 :shelf-slope 1.5 :rate 44100}))

;; lowpass
(def effect2 (s/effect :simple-lowpass {:rate 44100 :cutoff 1000}))

;; 3 x lowpass
(def effect3 (s/compose-effects effect2 effect2 effect2))

;; filter with dj-eq
(time (p/set-canvas-pixels! cnvs (p/filter-channels p/normalize (p/filter-channels (sig/effects-filter effect1 {:coding :alaw-rev} {:coding :ulaw}) p))))

;; filter with 3 lowpass
(time (p/set-canvas-pixels! cnvs (p/filter-channels (sig/effects-filter effect3 {:signed? true} {:signed? true}) p)))

;; filter with all in YPbPr colorspace
(time (let [filter (sig/effects-filter (s/compose-effects effect1 effect2))
            res (->> p
                     (p/filter-colors c/to-YPbPr*)
                     (p/filter-channels filter)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize)
                     (p/filter-colors c/from-YPbPr*))]
        (p/set-canvas-pixels! cnvs res)))

(time (let [filter (sig/effects-filter (s/effect :divider {:denominator 2}))
            res (->> p
                     (p/filter-colors c/to-OHTA*)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize)
                     (p/filter-channels filter)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize)
                     (p/filter-colors c/from-YPbPr*))]
        (p/set-canvas-pixels! cnvs res)))

;; full process without use of filter-channels
(time (let [eff (s/effect :biquad-eq {:fc 2000 :gain -30 :bw 1 :fs 100000})
            resp (sig/apply-effects-to-pixels eff
                                              {:planar? false
                                               :coding :alaw
                                               :signed? true
                                               :channels [2 0 1]
                                               :bits 16}
                                              {:planar? false
                                               :coding :alaw-rev
                                               :signed? true
                                               :channels [2 0 1]
                                               :bits 16} p)]
        (p/set-canvas-pixels! cnvs (p/filter-channels p/normalize resp))))

;; fm filter
(time (let [eff (sig/effects-filter (s/effect :fm {:quant 10 :omega (* m/TWO_PI 0.00225857) :phase 0.00822}) (c2d/width p))
            res (p/filter-channels eff p)]
        (p/set-canvas-pixels! cnvs res)))

;; Echo uses array of doubles internally to keep the state, don't use one effect in multiple threads. Filter-channels runs the same effect (with one state) in 3 threads for red, green and blue channels separately.
;; Wrong way (run multiple times)
(time (let [eff (sig/effects-filter (s/effect :echo))
            res (p/filter-channels eff p)]
        (p/set-canvas-pixels! cnvs res)))

;; Good way
(time (let [effect-r (sig/effects-filter (s/effect :echo))
            effect-g (sig/effects-filter (s/effect :echo))
            effect-b (sig/effects-filter (s/effect :echo))
            res (p/filter-channels effect-r effect-g effect-b nil p)]
        (p/set-canvas-pixels! cnvs res)))

;; vcf303 normalize (or equalize) after filtering
(time (let [eff (sig/effects-filter (s/effect :vcf303 {:trigger true}) (* 50 ^long (c2d/width p)))
            res (p/filter-channels p/equalize (p/filter-channels eff p))]
        (p/set-canvas-pixels! cnvs res)))


(time (let [eff (s/effect :slew-limit {:maxrise 50 :maxfall 1000})
            res (p/filter-channels p/normalize (sig/apply-effects-to-pixels eff {:signed? true} {:signed? true} p))]
        (p/set-canvas-pixels! cnvs res)))

(time (let [eff (s/effect :mda-thru-zero {:speed 0.41 :depth 0.4 :feedback 0.2 :depth-mod 0.1 :mix 0.5})
            res (p/filter-channels p/normalize (sig/apply-effects-to-pixels eff {:signed? true} {:signed? true} p))]
        (p/set-canvas-pixels! cnvs res)))

;; saving and loading signal to and from file
(s/save-signal (sig/pixels->signal p {:planar? false
                                      :coding :alaw
                                      :signed? true
                                      :channels [2 0 1]
                                      :bits 16}) "results/ex16/signal.raw")

(s/load-signal "results/ex16/signal.raw")
