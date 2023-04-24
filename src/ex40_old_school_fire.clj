;; http://lodev.org/cgtutor/fire.html

(ns ex40-old-school-fire
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const bw 400)
(def ^:const bh 200)
(def ^:const bh- (dec bh))

(def palette (mapv #(c/from-HSL* (c/color (/ ^long % 3.0) 255.0 (min 255.0 (* 2.0 ^long %)))) (range 256)))

(defn make-2d-getter-setter
  "2d `ints` array getter"
  [^long w ^long h]
  (let [^ints array (int-array (* bw bh))]
    [(fn ^long [^long x ^long y] (aget array (+ (m/wrap 0 w x) (* (m/wrap 0 h y) w))))
     (fn ^long [^long x ^long y ^long v] (aset array (+ (m/wrap 0 w x) (* (m/wrap 0 h y) w)) (unchecked-int v)))]))

(defn process-fire
  "Calculate next step"
  [[get2d set2d]]
  (dotimes [x bw]
    (set2d x bh- (r/randval 0.8 (r/irand 256) 0))) 
  (dotimes [y bh-]
    (let [y+ (inc y)]
      (dotimes [x bw]
        (set2d x y (/ (bit-shift-left (+ ^long (get2d (dec x) y+)
                                         ^long (get2d x y+)
                                         ^long (get2d (inc x) y+)
                                         ^long (get2d x (inc y+))) 6) 257))))))

(defn set-pixels
  "buffer -> pixels"
  [pixels [get2d _]] 
  (dotimes [x bw]
    (dotimes [y bh]
      (p/set-color! pixels x y (palette (get2d x y)))))
  pixels)

(defn draw
  "Frames"
  [canvas _ _ state]
  (let [[pixels fns :as all] (or state [(p/pixels bw bh) (make-2d-getter-setter bw bh)])]
    (process-fire fns)
    (p/set-canvas-pixels! canvas (set-pixels pixels fns))
    all))

(def cnvs (c2d/canvas bw bh :mid))
(def window (c2d/show-window cnvs "Oldschool fire" (* 2 bw) (* 2 bh) 60 draw))

(defmethod c2d/key-pressed [(:window-name window) \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex40/" ".jpg")))
