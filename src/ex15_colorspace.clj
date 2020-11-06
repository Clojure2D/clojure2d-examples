(ns ex15-colorspace
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; convert image into some colorspaces

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def cnvs (c2d/canvas (c2d/width img) (c2d/height img)))
(def window (c2d/show-window cnvs "Colorspace" 15 nil))

(defmethod c2d/key-pressed ["Colorspace" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex15/" ".jpg")))

(p/set-canvas-pixels! cnvs (p/filter-colors c/to-OHTA* img))
(p/set-canvas-pixels! cnvs (p/filter-colors c/to-CMY* img))
(p/set-canvas-pixels! cnvs (p/filter-colors c/to-YPbPr* img))
(p/set-canvas-pixels! cnvs (p/filter-colors c/to-Gray* img))
(p/set-canvas-pixels! cnvs (p/filter-colors c/from-OHTA* img))
(p/set-canvas-pixels! cnvs (p/filter-colors c/from-YPbPr* img))

(p/set-canvas-pixels! cnvs (p/filter-colors (comp c/to-YPbPr* c/from-OHTA* c/from-YPbPr*) img))

;; equalize histogram in YPbPr colorspace
(p/set-canvas-pixels! cnvs (->> img
                                (p/filter-colors c/to-YPbPr*)
                                (p/filter-channels p/equalize)
                                (p/filter-colors c/from-YPbPr*)))


;; random conversion
(let [cs1 (rand-nth c/colorspaces-list)
      cs2 (rand-nth c/colorspaces-list)]
  (println (str ":RGB -> " cs1 " -> " cs2 " -> :RGB"))
  (p/set-canvas-pixels! cnvs (->> img
                                  (p/filter-colors (first (c/colorspaces* cs1)))
                                  (p/filter-channels p/normalize)
                                  (p/filter-colors (second (c/colorspaces* cs2)))
                                  (p/filter-channels p/normalize))))

