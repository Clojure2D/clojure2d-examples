(ns ex20-colors
  (:require [clojure2d.color :as c]
            [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p]
            [fastmath.core :as m])
  (:import fastmath.vector.Vec4
           clojure2d.pixels.Pixels))

;; reduce colors to random palette

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def cnvs (canvas (width img) (height img)))
(def window (show-window cnvs "Colors" 15 nil))

(defmethod key-pressed ["Colors" \space] [_ _]
  (save cnvs (next-filename "results/ex20/" ".jpg")))

;; colourlovers
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter (rand-nth (concat (vals c/palette-presets) c/colourlovers-palettes))) img))

;; generated palette with 8 colors
(let [random-palette-8 (m/sample (c/iq-palette-random-gradient) 8)]
  (p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter random-palette-8) img)))

;; different distance function
(def random-palette-6 (conj (m/sample (c/iq-palette-random-gradient) 4) (Vec4. 0 0 0 255.0) (Vec4. 255 255 255 255)))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-abs random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-cheb random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-cos random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-emd random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-canberra random-palette-6) img))
(p/set-canvas-pixels! cnvs (p/filter-colors (c/make-reduce-color-filter v/dist-discrete random-palette-6) img))

(let [paletton-palette (c/paletton :triad 0 {:compl true :angle 20 :preset (rand-nth c/paletton-presets-list)})]
  (p/set-canvas-pixels! cnvs  (p/filter-colors (c/make-reduce-color-filter paletton-palette) img)))
