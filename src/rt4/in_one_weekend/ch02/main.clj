(ns rt4.in-one-weekend.ch02.main
  (:require [fastmath.core :as m]
            [rt4.common :as common]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long image-width 256)
(def ^:const ^long image-height 256)

;; precompute
(def ^:const ^long image-width- (dec image-width))
(def ^:const ^long image-height- (dec image-height))

(def image (common/make-pixels-and-show image-width image-height))

(common/pdotimes [j image-height]
  (dotimes [i image-width]
    (let [r (/ (double i) image-width-)
          g (/ (double j) image-height-)]
      (p/set-color! (:pixels image) i (- image-height- j) (c/scale-up [r g 0.25])))))

(comment
  (common/save image "results/rt4/in_one_weekend/ch02.jpg"))
