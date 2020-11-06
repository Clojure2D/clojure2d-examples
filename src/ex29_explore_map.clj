(ns ex29-explore-map
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.complex :as c]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def cnvs (c2d/canvas 600 600))
(def window (c2d/show-window cnvs "Explore map"))

(defmethod c2d/key-pressed ["Explore map" \space] [_ _]
  (binding [c2d/*jpeg-image-quality* 0.9]
    (c2d/save cnvs (c2d/next-filename "results/ex29/" ".jpg"))))

(defn make-standard-map
  "Standard Map"
  [^double K]
  (fn [^double I ^double Theta]
    (let [I' (mod (+ I (* K (m/sin Theta))) m/TWO_PI)
          Theta' (mod (+ Theta I') m/TWO_PI)]
      [I' Theta'])))

(defn make-henon-quadratic-map
  "Henon's quadratic map"
  [^double a]
  (let [sina (m/sin a)
        cosa (m/cos a)]
    (fn [^double x ^double y] 
      (let [x2 (* x x)
            x' (- (* x cosa) (* sina (- y x2)))
            y' (+ (* x sina) (* cosa (- y x2)))]
        [x' y']))))

(defn make-henon-map
  "Henon map"
  [^double alpha ^double beta]
  (fn [^double x ^double y]
    [(+ (- 1.0 (* alpha x x)) y)
     (* beta x)]))

(defn make-gingerbreadman-map
  "Gingerbreadman map"
  [^double a ^double b]
  (fn [^double x ^double y]
    [(+ (- 1.0 (* a y)) (* b (m/abs x)))
     x]))

(defn make-ikeda-map
  "Ikeda map"
  [^double u]
  (fn [^double x ^double y]
    (let [t (- 0.4 (/ 6.0 (inc (+ (* x x) (* y y)))))
          sint (m/sin t)
          cost (m/cos t)]
      [(inc (* u (- (* x cost) (* y sint))))
       (* u (+ (* x sint) (* y cost)))])))

(defn make-exponential-map
  "Exp map"
  [^double rec ^double imc]
  (let [^Vec2 c (Vec2. rec imc)]
    (fn [^double re ^double im]
      (let [^Vec2 z (Vec2. re im)
            ^Vec2 res (c/mult c (c/exp z))]
        [(.x res) (.y res)]))))

(defn gumo-mira-fn
  "Gumowski-Mira function"
  ^double [^double A ^double x]
  (let [x2 (* x x)]
    (+ (* A x) (/ (* 2.0 (- 1.0 A) x2) (m/sq (inc x2))))))


(defn make-gumo-mira-map
  "Gumowski Mira"
  [^double A ^double B]
  (fn [^double x ^double y]
    (let [x' (+ (* B y) (gumo-mira-fn A x))
          y' (- (gumo-mira-fn A x') x)]
      [x' y'])))

(defn draw-map-position
  "draw N points from given starting point"
  [canvas initx inity [sminx smaxx sminy smaxy] f]
  
  (loop [[x y] [initx inity]
         count (int 0)]
    (when (and (c2d/window-active? window) (< count 3000))
      (let [xx (m/norm x sminx smaxx 0 600)
            yy (m/norm y sminy smaxy 0 600)]

        (when (> count 20)
          (c2d/point canvas xx yy))
        
        (recur (f x y)
               (unchecked-inc count))))))

(defn draw-map
  "draw map from starting point grid"
  [canvas [rmin rmax ^double step] [rminx rmaxx rminy rmaxy] scale f]
  (c2d/set-background canvas 10 10 10)
  (c2d/set-color canvas 240 240 240 20)
  (doseq [o (map #(+ (* 2.0 step ^double (r/grand)) ^double %) (range rmin rmax step))]
    (draw-map-position canvas o o scale f) ;; initial point on diagonal line
    (draw-map-position canvas (r/drand rmin rmax) (r/drand rmin rmax) scale f))) ;; random initial point

(def maps {:standard-map [[0 m/TWO_PI 0.015] [0.0 m/TWO_PI 0.0 m/TWO_PI] [0.0 m/TWO_PI 0.0 m/TWO_PI]
                          make-standard-map [[-5 5]]]
           :henon-quadratic-map [[-2.0 2.0 0.01] [-2.0 2.0 -2.0 2.0] [-1.5 1.5 -1.2 1.8]
                                 make-henon-quadratic-map [[0.5 5.0]]]
           :henon-map [[-2.0 2.0 0.003] [-2.0 2.0 -2.0 2.0] [-5.0 5.0 -5.0 5.0]
                       make-henon-map [[0.1 1.05] [0.7 0.9999]]]
           :gingerbreadman-map [[-5.0 10.0 0.01] [-5.0 10.0 -5.0 10.0] [-10.0 20.0 -10.0 20.0]
                                make-gingerbreadman-map [[0.75 0.9999] [0.6 1.5]]]
           :ikeda-map [[-5.0 10.0 0.01] [-5.0 10.0 -5.0 10.0] [-5.0 10.0 -5.0 10.0]
                       make-ikeda-map [[0.9 0.9999]]]
           :exponential-map [[-2.0 2.0 0.002] [-2.0 2.0 -2.0 2.0] [-2.0 2.0 -1.0 3.0]
                             make-exponential-map [[0.6 1.05] [0.2 1.2]]]
           :gumowski-mira-map [[-1.0 1.0 0.003] [-1.0 1.0 -1.0 1.0] [-3.0 3.0 -3.0 3.0]
                               make-gumo-mira-map [[-1.0 1.0] [0.7 1.05]]]})

(let [random-map (rand-nth (keys maps))
      [steps r scale f pars] (random-map maps)
      rpars (map #(apply r/drand %) pars)]
  (println random-map)
  (println rpars)
  (c2d/with-canvas-> cnvs
    (draw-map steps r scale (apply f rpars)))
  :done)

