(ns ex47-nebula
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const size 800)

(def ^:const gamma (/ 0.85))
(def ^:const cgamma (/ 2.0))
(def ^:const vibrancy 0.5)
(def ^:const brightness 1.6)
(def ^:const contrast 1.2)
(def ^:const saturation 1.2)
(def ^:const steps 100000)

(def col (c/gradient [0x6699cc 0xaa6633 0xffdd88]))

(defn draw-nebula []
  (let [b (p/renderer size size :hann)]
    (dotimes [_ steps]
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
        (p/set-color! b px py (c/clamp (col t)))))
    b))

(def c (c2d/canvas size size))
(def buff (p/renderer size size))
(def window (c2d/show-window {:window-name "Nebula"
                            :canvas c}))

(defmethod c2d/key-pressed ["Nebula" \space] [_ _]
  (c2d/save c "results/ex47/nebula.jpg"))

(loop [iter (int 1)]
  (let [ts (mapv (fn [_] (future (draw-nebula))) (range c2d/available-cores))]
    (apply p/merge-renderers buff (map deref ts))
    (println (str "Points: " (* c2d/available-cores iter steps)))
    (p/set-canvas-pixels! c (p/to-pixels buff {:background (c/gray 15)
                                               :gamma-alpha gamma
                                               :gamma-color cgamma
                                               :vibrancy vibrancy
                                               :brightness brightness
                                               :contrast contrast
                                               :saturation saturation})))
  (if (c2d/window-active? window) (recur (inc iter)) (println "finished")))

