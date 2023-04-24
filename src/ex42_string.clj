(ns ex42-string
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.fields :as vr]
            [clojure.pprint :refer [pprint]]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const w 500)
(def ^:const h 500)
(def ^:const midh (/ h 2))

(def ^:const x1 -2.5)
(def ^:const y1 -2.5)
(def ^:const x2 2.5)
(def ^:const y2 2.5)
(def ^:const step (/ (- x2 x1) 51))
(def ^:const lines 15)
(def ^:const lines- (dec 15))

(def ^:const x1- (dec x1))
(def ^:const y1- (dec y1))
(def ^:const x2+ (inc x2))
(def ^:const y2+ (inc y2))

(def ^:const fscale 0.7)

(defn draw
  [canvas window ^long frame _]
  (let [field (c2d/get-state window)
        s (m/mnorm (max 0 (c2d/mouse-x window)) 0.0 w 10.0 200.0)
        t (/ frame 100.0)]

    (-> canvas
        (c2d/set-background :black 50)
        (c2d/set-color :white 80))
    
    (dotimes [yl lines]
      (let [y (m/norm yl 0 lines- y1 y2)
            p (for [x (range x1 (+ x2 step) step) 
                    :let [nv (Vec2. (+ ^double x (- ^double (r/noise x y t) 0.5)) y)
                          cx (m/norm (m/cos (m/norm x x1 x2 (- m/PI) m/PI)) -1.0 1.0 0.0 s)
                          ^Vec2 vv (v/add nv (v/limit (v/mult (field nv) cx) 200))
                          xx (m/norm (+ ^double x (m/constrain (.x vv) -0.1 0.1)) x1- x2+ 0.0 w)]]
                (Vec2. xx (+ midh (.y vv))))]
        (c2d/path-bezier canvas p false true))))

  ;; (save canvas (next-filename "generateme/frames42/f" ".jpg"))

  )

(defn new-state
  "Create new combination."
  []
  (binding [vr/*skip-random-fields* true]
    (let [field-config (vr/random-configuration 3)]
      (pprint field-config)
      (vr/combine field-config))))

(c2d/close-session)
(def window (c2d/show-window {:canvas (c2d/canvas w h)
                            :window-name "String"
                            :state (new-state)
                            :draw-fn draw}))

(defmethod c2d/mouse-event [(:window-name window) :mouse-clicked] [_ _]
  (new-state))

(defmethod c2d/key-pressed [(:window-name window) \space] [_ s]
  (c2d/save window (c2d/next-filename "results/ex42/" ".jpg"))
  s)
