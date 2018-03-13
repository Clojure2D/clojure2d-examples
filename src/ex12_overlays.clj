;; three overlays I defined (used in my works)

(ns examples.ex12-overlays
  (:require [clojure2d.core :as core]
            [clojure2d.extra.overlays :as o])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; load image and store
(def img (core/load-image "results/test.jpg"))

(def canvas (core/canvas (core/width img) (core/height img)))
(def window (core/show-window canvas "Overlays" 15 nil))

(defmethod core/key-pressed ["Overlays" \space] [_ _]
  (core/save canvas (core/next-filename "results/ex12/" ".jpg")))

;; image
(core/with-canvas-> canvas
  (core/image img))

;; tv/rgb skanning lines
(core/with-canvas-> canvas
  (core/image (o/render-rgb-scanlines img)))

;; noise
(def noise-overlay (o/make-noise (core/width img) (core/height img) {:alpha 80}))

(core/with-canvas-> canvas
  (core/image (o/render-noise img noise-overlay)))

;; spots, it's good to prepare overlay first, than apply onto the image
q(def spots-overlay (o/make-spots (core/width img) (core/height img) {:alpha 80 :intensities [30 60 120 180 220]}))

(core/with-canvas-> canvas
  (core/image (o/render-spots img spots-overlay)))

;; apply all
(core/with-canvas-> canvas
  (core/image (-> img
                  (o/render-noise noise-overlay)
                  (o/render-spots spots-overlay)
                  (o/render-rgb-scanlines))))

;; crt scanlines
(core/with-canvas-> canvas
  (core/image (o/render-crt-scanlines img)))

;; crt scanlines with mask
(core/with-canvas-> canvas
  (core/image (o/render-crt-scanlines img {:resolution 4 :mask-light 1.5})))
