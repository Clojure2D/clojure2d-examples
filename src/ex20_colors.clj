(ns ex20-colors
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p])
  (:import clojure2d.pixels.Pixels))

;; reduce colors to random palette

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def cnvs (c2d/canvas (c2d/width img) (c2d/height img)))
(def window (c2d/show-window cnvs "Colors" 15 nil))

(defmethod c2d/key-pressed ["Colors" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex20/" ".jpg")))

;; colourlovers
(p/set-canvas-pixels! cnvs (p/filter-colors (c/nearest-color (c/random-palette)) img))

;; generated palette with 8 colors
(let [random-palette-8 (c/palette (c/random-gradient) 8)]
  (p/set-canvas-pixels! cnvs (p/filter-colors (c/nearest-color random-palette-8) img)))

;; different distance function
(def random-palette-6 (c/palette (c/random-palette) 6))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-abs) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-cheb) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-cos) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-emd) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-canberra) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % v/dist-discrete) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/delta-c) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/delta-h) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/delta-e-cie) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/delta-e-cmc) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/euclidean) img))
(p/set-canvas-pixels! cnvs (p/filter-colors #(c/nearest-color random-palette-6 % c/contrast-ratio) img))

(let [paletton-palette (c/paletton :triad 0 {:compl true :angle 20 :preset (rand-nth c/paletton-presets-list)})]
  (p/set-canvas-pixels! cnvs  (p/filter-colors (c/nearest-color paletton-palette) img)))
