(ns ex13-segmentations
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as clr]
            [clojure2d.extra.segmentation :as segm]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long min-size 4) ; minimal block size
(def ^:const ^long max-size 32) ; maximum block size
(def ^:const ^double threshold 15.0) ; dividing threshold
(def ^:const ^long channel 1) ; channel to operate with

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (core/canvas (core/width img) (core/height img)))
(def window (core/show-window canvas "Segmentation" 15 nil))

(defmethod core/key-pressed ["Segmentation" \space] [_ _]
  (core/save canvas (core/next-filename "results/ex13/" ".jpg")))

(defn example-13
  "segment image based on selected channel, color segments and save"
  ([canvas]
   (example-13 canvas :default))
  ([canvas strategy]
   (binding [p/*pixels-edge* 128] ; let's be sure we have some fixed value outside the image
     (let [segm (segm/segment-pixels img channel {:min-size min-size :max-size max-size :threshold threshold})
           iter (core/make-counter 0)

           draw (fn [canv] (doseq [[x y size] segm]
                             (let [defcol (clr/awt-color (p/get-value img 0 x y)
                                                         (p/get-value img 1 x y)
                                                         (p/get-value img 2 x y))
                                   col (condp = strategy
                                         :bw (if (even? (iter))
                                               (clr/awt-color 0 0 0)
                                               (clr/awt-color 255 255 255))
                                         :size (let [g (int (m/cnorm (m/logb 2 size) 0 6 5 255))]
                                                 (clr/awt-color g g g))
                                         defcol)]
                               (core/set-awt-color canv col)
                               (core/rect canv x y size size))))]

       (core/with-canvas-> canvas
         (draw))))))

;; color with image colors
(example-13 canvas)

;; color black and white
(example-13 canvas :bw)

;; color depends on size
(example-13 canvas :size)

(binding [segm/*sequence-generator* (r/jittered-sequence-generator :r2 2 0.5)]
  (example-13 canvas :size))
