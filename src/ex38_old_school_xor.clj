(ns ex38-old-school-xor
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 600 600 :mid))

(defn draw-rings
  "Draw rings"
  [canvas posx posy]
  (dotimes [x 17]
    (let [size (* (inc x) 80)]
      (c2d/ellipse canvas posx posy size size true)))
  canvas)

(defn draw
  "Frames"
  [canvas _ ^long fps _]
  (let [t (/ fps 60.0) 
        sa (m/mnorm (m/qsin (* 0.5 t)) -1.0 1.0 100.0 500.0)
        ca (m/mnorm (m/qcos t) -1.0 1.0 100.0 500.0)
        sb (m/mnorm (m/qsin (inc t)) -1.0 1.0 100.0 500.0)
        cb (m/mnorm (m/qcos (* 2.0 (dec t))) -1.0 1.0 200.0 500.0)]
    (-> canvas
        (c2d/set-background :black)
        (c2d/set-color :white)
        (c2d/xor-mode :black)
        (c2d/set-stroke 20)
        (draw-rings sb cb)
        (draw-rings sa ca))))

(def window (c2d/show-window cnvs "Oldschool XOR" #(draw %1 %2 %3 %4)))

(defmethod c2d/key-pressed [(:window-name window) \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex38/" ".jpg")))
