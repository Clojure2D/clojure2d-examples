;; filtering and blending pixels

(ns ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as b])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; first let's load an image into ^Pixels type
;; pixels are layout in planar mode (first red channel, then green, blue and alpha)
(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (core/canvas (core/width img) (core/height img)))
(def window (core/show-window canvas "Pixels" 15 nil))

(defmethod core/key-pressed ["Pixels" \space] [_ _]
  (core/save canvas (core/next-filename "results/ex11/" ".jpg")))

;; now let's process pixels with defined pixels

;; dilate three times on 3 channels, skip alpha
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/dilate)
                                  (p/filter-channels p/dilate)
                                  (p/filter-channels p/dilate)))

;; erode three times on 3 channels, skip alpha
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/erode)
                                  (p/filter-channels p/erode)
                                  (p/filter-channels p/erode)))

;; box blur with radius 5
(p/set-canvas-pixels! canvas (p/filter-channels p/box-blur-5 img))

;; box blur with radius 23
(p/set-canvas-pixels! canvas (p/filter-channels (p/box-blur 23) img))

;; gaussian blur with radius 5
(p/set-canvas-pixels! canvas (p/filter-channels p/gaussian-blur-5 img))

;; posterize 4, to give number of levels call (p/make-posterize 2)
(p/set-canvas-pixels! canvas (p/filter-channels p/posterize-4 img))

;; threshold all channels but alpha (50%), custom threshold (p/make-threshold 44)
(p/set-canvas-pixels! canvas  (p/filter-channels p/threshold-50 img))

;; median filter five times
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/median)
                                  (p/filter-channels p/median)
                                  (p/filter-channels p/median)
                                  (p/filter-channels p/median)
                                  (p/filter-channels p/median)))

;; quntile filter 80 times, you can select quantile 0-8, 4 = median filter
(p/set-canvas-pixels! canvas (reduce (fn [res _] (p/filter-channels p/quantile-3 res)) img (range 80)))

;; tint image
(p/set-canvas-pixels! canvas (p/filter-channels (p/tint (c/color 10 30 200)) img))

;; modulate channels (in HSB colorspace don't touch hue, lower saturation and brightness a little bit)
(p/set-canvas-pixels! canvas (->> img
                                  (p/filter-colors c/to-HSB*)
                                  (p/filter-channels nil (p/modulate 0.5) (p/modulate 0.5) nil)
                                  (p/filter-colors c/from-HSB*)))

;; let's compose some images

;; let's modulo add two images
(p/set-canvas-pixels! canvas (p/compose-channels :madd false
                                                 (p/load-pixels "results/ex11/1FABF63A_000034.jpg")
                                                 (p/load-pixels "results/ex11/1FABF63A_000039.jpg")))

;; let's make random blends
(p/set-canvas-pixels! canvas (p/compose-channels (rand-nth b/blends-list) (rand-nth b/blends-list) (rand-nth b/blends-list) nil
                                                 (p/load-pixels "results/ex11/1FABF63A_000034.jpg")
                                                 (p/load-pixels "results/ex11/1FABF63A_000039.jpg")))
