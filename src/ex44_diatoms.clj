(ns ex44-diatoms
  "Diatoms/rosettes from noise and vector fields.

  * click to change settings
  * c - to change color palette"
  (:require [clojure2d.core :refer :all]
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

(def ^:const ^int w 1000)
(def ^:const ^int h 1000)

(def ^:const ^int hw (/ w 2))
(def ^:const ^int hh (/ w 2))
(def ^:const ^int bw (/ w 3))
(def ^:const ^int bh (/ w 3))
(def ^:const ^int bw- (- bw))
(def ^:const ^int bh- (- bh))
(def ^:const ^double bw15 (* 1.5 bw))
(def ^:const ^double bh15 (* 1.5 bh))
(def ^:const ^double bw15- (- bw15))
(def ^:const ^double bh15- (- bh15))

(def ^:const renderer-config {:saturation 1.2
                              :brightness 1.2
                              :gamma-alpha 0.9
                              :gamma-color 1.05
                              :vibrancy 0.2})

(defn random-config
  ""
  []
  {:noise (r/random-noise-fn)
   :field (f/combine)
   :add? (r/brand)
   :grad (c/random-gradient)
   :rots (r/irand 2 10)
   :bins (p/renderer w h :gaussian)})

(defn variant-1
  ""
  [x y n ^double h]
  (v/vec2 (m/mnorm (m/sin (+ ^double (n x y) h)) -1.0 1.0 bw- bw)
          (m/mnorm (m/sin (+ ^double (n y x) h)) -1.0 1.0 bh- bh)))

(defn variant-2
  ""
  [x y n ^double h]
  (v/vec2 (m/mnorm (m/sin (* ^double (n x y) h)) -1.0 1.0 bw15- bw15)
          (m/mnorm (m/sin (* ^double (n y x) h)) -1.0 1.0 bh15- bh15)))


(defn draw
  ""
  [canvas window frame _]
  (let [{:keys [noise field add? ^int rots grad bins]} (get-state window)]
    (dotimes [i 100000]
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
    (image canvas (p/to-pixels bins renderer-config))))

(def cnvs (canvas w h))
(def window (show-window {:canvas cnvs
                          :state (random-config)
                          :draw-fn draw
                          :window-name "ex44"}))

(defmethod mouse-event ["ex44" :mouse-pressed] [_ _] (random-config))

(defmethod key-pressed ["ex44" \c] [_ s] (assoc s :bins (p/renderer w h) :grad (c/random-gradient)))
(defmethod key-pressed ["ex44" \space] [_ s] (save cnvs (next-filename "results/ex44/" ".jpg")) s)
