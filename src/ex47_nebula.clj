(ns examples.ex47-nebula
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int size 800)

(def ^:const ^double gamma (/ 0.85))
(def ^:const ^double cgamma (/ 2.0))
(def ^:const ^double vibrancy 0.5)
(def ^:const ^double brightness 1.6)
(def ^:const ^double contrast 1.2)
(def ^:const ^double saturation 1.2)
(def ^:const ^int steps 1000000)

(def col (c/gradient [0x6699cc 0xaa6633 0xffdd88]))

(defn draw-nebula []
  (let [b (p/renderer size size :hann)]
    (dotimes [i steps]
      (let [x (r/grand)
            y (r/grand)
            r (* 50.0 (m/hypot-sqrt x y))
            t1 (r/noise x y)
            t2 (r/noise (- y 1.1) (- x 1.1) 0.4)
            nx (* r (- t1 0.5))
            ny (* r (- t2 0.5))
            px (+ 400.0 (* 100.0 x) nx)
            py (+ 400.0 (* 100.0 y) ny)
            t (m/sqrt (* t1 t2))]
        (p/set-color! b px py (col t))))
    b))

(def c (canvas size size))
(def buff (p/renderer size size))
(def window (show-window {:window-name "Nebula"
                          :canvas c}))

(defmethod key-pressed ["Nebula" \space] [_ _]
  (save c "results/ex47/nebula.jpg"))

(loop [iter (int 1)]
  (let [ts (mapv (fn [_] (future (draw-nebula))) (range available-cores))]
    (apply p/merge-renderers buff (map deref ts))
    (println (str "Points: " (* available-cores iter steps)))
    (p/set-canvas-pixels! c (p/to-pixels buff {:background (c/gray 15)
                                               :gamma-alpha gamma
                                               :gamma-color cgamma
                                               :vibrancy vibrancy
                                               :brightness brightness
                                               :contrast contrast
                                               :saturation saturation})))
  (when (window-active? window) (recur (inc iter))))

