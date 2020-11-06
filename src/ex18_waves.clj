;; visualize wave generators

(ns ex18-waves
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.signal :as s]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 600 600))

(def display (c2d/show-window cnvs "waves"))

(defmethod c2d/key-pressed ["waves" \space] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex18/" ".jpg")))

;; frequencies and amplitudes
(def f (mapv #(<< 1 ^long %) (range 16)))
(def a (mapv #(/ 1.0 ^long %) f))

(defn draw-fun
  ""
  [canvas f]
  (let [yfn #(+ 300.0 (* 300.0 ^double (f (/ ^double % 600.0))))]
    (loop [x (int 1)
           prev (yfn 0)]
      (when (< x 600)
        (let [ny (yfn x)]
          (c2d/line canvas x prev (inc x) ny)
          (recur (inc x) ny))))))

(def valid-oscillators (vec (disj (set s/oscillators) :constant)))

;; run several times
(let [lst (map #(s/oscillator (rand-nth valid-oscillators) (f %) (a %) (r/drand 1)) (range 1 5))]
  (c2d/with-canvas-> cnvs
    (c2d/set-color :white)
    (c2d/set-background :black)
    (draw-fun (apply s/oscillators-sum lst)))
  :done)

(defn draw-fun2
  ""
  [canvas y f]
  (dotimes [x 600]
    (let [^double v (-> (f (m/norm x 0 600 0.0 1.0)) 
                        (m/cnorm -1.0 1.0 0.0 1.0))
          v1 (* 255.0 (m/sq v))
          v2 (* 255.0 (m/sqrt v))
          c (c/color (* 255.0 v) v1 v2)]
      (c2d/set-color canvas c)
      (c2d/rect canvas x y 1 1))))

;; try several times
(let [num 7
      wvs (repeatedly num #(rand-nth valid-oscillators))
      phases (repeatedly num #(r/drand 1.0))
      phasemult (repeatedly num #(r/drand -3.0 3.0))
      octaves (repeatedly num #(r/irand num))]
  (dotimes [y 600]
    (let [yy (/ y 600.0)
          lst (map #(s/oscillator (nth wvs %) (f (nth octaves %)) (a (nth octaves %)) (+ (* yy ^double (nth phasemult %)) ^double (nth phases %))) (range num))]
      (c2d/with-canvas-> cnvs
        (draw-fun2 y (apply s/oscillators-sum lst)))))
  :done)


;; save signal to file
;; open in Audacity as RAW 16 bit signed, mono, big-endian, 44100Hz
(let [num 10
      amp (* 1.5 (/ 1.0 num))
      lst (map #(s/oscillator (rand-nth valid-oscillators) (* 150 ^long %) amp (r/drand 1)) (range 1 (inc num)))
      f (apply s/oscillators-sum lst)]
  (s/save-signal (s/oscillator->signal f 44100 10) "results/ex18/wave.raw"))
