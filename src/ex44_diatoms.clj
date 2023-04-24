(ns ex44-diatoms
  "Diatoms/rosettes from noise and vector fields.

  * click to change settings
  * c - to change color palette"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.fields :as f]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const w 1000)
(def ^:const h 1000)

(def ^:const hw (/ w 2))
(def ^:const hh (/ w 2))
(def ^:const bw (/ w 3))
(def ^:const bh (/ w 3))
(def ^:const bw- (- bw))
(def ^:const bh- (- bh))
(def ^:const bw15 (* 1.5 bw))
(def ^:const bh15 (* 1.5 bh))
(def ^:const bw15- (- bw15))
(def ^:const bh15- (- bh15))

(def ^:const renderer-config {:saturation 1.2
                              :brightness 1.2
                              :gamma-alpha 0.9
                              :gamma-color 1.05
                              :vibrancy 0.2})

(defn random-config
  []
  {:noise (r/random-noise-fn)
   :field (f/combine)
   :add? (r/brand)
   :grad (c/random-gradient)
   :rots (r/irand 2 10)
   :bins (p/renderer w h :gaussian)})

(defn variant-1
  [x y n ^double h]
  (v/vec2 (m/mnorm (m/sin (+ ^double (n x y) h)) -1.0 1.0 bw- bw)
          (m/mnorm (m/sin (+ ^double (n y x) h)) -1.0 1.0 bh- bh)))

(defn variant-2
  [x y n ^double h]
  (v/vec2 (m/mnorm (m/sin (* ^double (n x y) h)) -1.0 1.0 bw15- bw15)
          (m/mnorm (m/sin (* ^double (n y x) h)) -1.0 1.0 bh15- bh15)))


(defn draw
  [canvas window _ _]
  (let [{:keys [noise field add? ^int rots grad bins]} (c2d/get-state window)]
    (dotimes [_ 100000]
      (let [xx (r/drand -1.0 1.0)
            yy (r/drand -1.0 1.0)
            res (-> (v/vec2 xx yy)
                    (v/mult 3.0)
                    field)
            xy (if add?
                 (variant-1 xx yy noise (* 0.5 (v/heading res)))
                 (variant-2 xx yy noise (m/sin (v/heading res))))
            col (m/abs (m/sin (v/magsq res)))
            afact (/ m/TWO_PI rots)]
        (doseq [angle (map #(* ^long % afact) (range rots))]
          (let [^Vec2 rot (v/rotate xy angle)]
            (p/set-color! bins (+ hw (.x rot)) (+ hh (.y rot)) (grad col))))))
    (c2d/image canvas (p/to-pixels bins renderer-config))))

(def cnvs (c2d/canvas w h))
(def window (c2d/show-window {:canvas cnvs
                            :state (random-config)
                            :draw-fn draw
                            :window-name "ex44"}))

(defmethod c2d/mouse-event ["ex44" :mouse-pressed] [_ _] (random-config))

(defmethod c2d/key-pressed ["ex44" \c] [_ s] (assoc s :bins (p/renderer w h) :grad (c/random-gradient)))
(defmethod c2d/key-pressed ["ex44" \space] [_ s] (c2d/save cnvs (c2d/next-filename "results/ex44/" ".jpg")) s)
