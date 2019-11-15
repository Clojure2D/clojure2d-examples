(ns ex43-noise-figures
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [fastmath.fields :as f]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn new-pair
  "Create new noise and field"
  []
  (let [ncfg (assoc (r/random-noise-cfg) :normalize? false :octaves (r/irand 1 5))
        fcfg (binding [f/*skip-random-fields* true] (f/random-configuration 2))]
    [(r/random-noise-fn ncfg) (f/combine fcfg)]))

(def sinusoidal (f/field :sinusoidal 0.2))

(defn draw
  ""
  [canvas window ^long frame _]
  (let [z (/ frame 1000.0)
        [noise-fn field-fn] (get-state window)
        scale (max 0.2 (* (mouse-x window) 0.001))
        hw (/ (width canvas) 2.0)
        hh (/ (height canvas) 2.0)
        speed (* z (max 1.0 (m/norm (mouse-y window) hh (height canvas) 1.0 8.0)))]
    (-> canvas
        (set-background :black 100)
        (translate hw hh)
        (rotate m/QUARTER_PI)
        (set-color (c/gray 250) 40))
    (dotimes [i 100000]
      (let [xx (r/drand (- scale) scale)
            yy (r/drand (- scale) scale)
            fv (sinusoidal (field-fn (v/vec2 xx yy)))
            xx (+ xx ^double (fv 0))
            yy (+ yy ^double (fv 1))
            nx (* 250 ^double (noise-fn xx yy speed))
            ny (* 250 ^double (noise-fn yy xx speed))]
        (point canvas nx ny)))))

(def window (show-window {:canvas (canvas 800 800)
                          :draw-fn draw
                          :state (new-pair)
                          :window-name "ex43"}))

(defmethod mouse-event ["ex43" :mouse-pressed] [_ _] (new-pair))
