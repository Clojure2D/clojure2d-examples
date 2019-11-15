(ns GG.P.P-1-2-2-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn sort-comparator
  "Compare colors"
  [f]
  (fn [c1 c2]
    (> ^double (f c1) ^double (f c2))))

(def hue-comparator (sort-comparator c/ch0))
(def saturation-comparator (sort-comparator c/ch1))
(def brightness-comparator (sort-comparator c/ch2))
(def luma-comparator (sort-comparator (comp c/luma c/from-HSB*)))

(defn load-img
  ""
  [image-name]
  (p/filter-colors c/to-HSB* (p/load-pixels image-name)))

(defn draw
  "Draw tiles"
  [canvas window _ _]
  (let [img (:image (get-state window))
        tile-count (/ (width canvas) (max (mouse-x window) 5))
        rect-size (/ (width canvas) (double tile-count))
        grid-seq (for [^long grid-y (range tile-count)
                       ^long grid-x (range tile-count)]
                   [(* grid-x rect-size) (* grid-y rect-size)])
        grid (map (fn [[px py]] (p/get-color img px py)) grid-seq)
        sgrid (case (:sort-mode (get-state window))
                :hue (sort hue-comparator grid)
                :saturation (sort saturation-comparator grid)
                :brightness (sort brightness-comparator grid)
                :grayscale (sort luma-comparator grid)
                grid)]
    (dorun (map #(let [[px py] %1]
                   (set-color canvas (c/from-HSB* %2))
                   (rect canvas px py rect-size rect-size)) grid-seq sgrid))))

(def window (show-window {:canvas (canvas 600 600)
                          :window-name "P_1_2_2_01"
                          :draw-fn draw
                          :state {:image (load-img "src/GG/data/pic1.jpg")}}))

(defmethod key-released [(:window-name window) \1] [_ s] (assoc s :image (load-img "src/GG/data/pic1.jpg")))
(defmethod key-released [(:window-name window) \2] [_ s] (assoc s :image (load-img "src/GG/data/pic2.jpg")))
(defmethod key-released [(:window-name window) \3] [_ s] (assoc s :image (load-img "src/GG/data/pic3.jpg")))

(defmethod key-released [(:window-name window) \4] [_ s] (assoc s :sort-mode nil))
(defmethod key-released [(:window-name window) \5] [_ s] (assoc s :sort-mode :hue))
(defmethod key-released [(:window-name window) \6] [_ s] (assoc s :sort-mode :saturation))
(defmethod key-released [(:window-name window) \7] [_ s] (assoc s :sort-mode :brightness))
(defmethod key-released [(:window-name window) \8] [_ s] (assoc s :sort-mode :grayscale))
