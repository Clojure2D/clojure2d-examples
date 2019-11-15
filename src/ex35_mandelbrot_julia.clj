;; Manderlbrot and Julia explorer
;;
;; Complex number can be described by 2x2 matrix with values set to:
;;
;; ```
;; (a+bi) = [ a -b
;;            b  a]
;; ```
;;
;; With normal matrix multiplication and addition as complex operations, absolute value is square root of determinant.
;;
;; Here I test various matrix representations (not only complex) to draw mandelbrots and julias
;;
;; Press:
;;
;; * SPACE to select random matrix representation
;; * 'm' for normal Mandelbrot
;;
;; Move mouse to see Julia related to mouse position

(ns ex35-mandelbrot-julia
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [clojure2d.pixels :as p])
  (:import [fastmath.vector Vec2 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 800)
(def ^:const ^int hw (/ w 2))

;; First define basic matrix operations and apply them to Vec4 type
(defprotocol Matrix
  (det [m])
  (inverse [m])
  (mulm [m1 m2]))

(extend Vec4
  Matrix
  {:det (fn [^Vec4 m]
          (- (* (.x m) (.w m)) (* (.y m) (.z m))))
   :mulm (fn [^Vec4 m1 ^Vec4 m2]
           (Vec4. (+ (* (.x m1) (.x m2))
                     (* (.y m1) (.z m2)))
                  (+ (* (.x m1) (.y m2))
                     (* (.y m1) (.w m2)))
                  (+ (* (.z m1) (.x m2))
                     (* (.w m1) (.z m2)))
                  (+ (* (.z m1) (.y m2))
                     (* (.w m1) (.w m2)))))
   :inverse (fn [^Vec4 m]
              (v/mult (Vec4. (.w m)
                             (- (.y m))
                             (- (.z m))
                             (.x m)) (/ ^double (det m))))})

(defn absm
  "Return absolute value of determinant."
  ^double [m]
  (m/abs (det m)))

(defn draw-mandelbrot
  "Draw Mandelbrot type"
  [canvas make-matrix]
  (dotimes [x w]
    (dotimes [y w]
      (let [xx (m/mnorm x 0.0 w -3.0 3.0)
            yy (m/mnorm y 0.0 w -3.0 3.0)
            sz (make-matrix 0.0 0.0)
            c (make-matrix xx yy)
            ^int idx (loop [iter (int 0)
                            z sz]
                       (if (and (< iter 512)
                                (< (absm z) 4.0))
                         (recur (inc iter) (v/add (mulm z z) c)) 
                         (dec iter)))
            col (* 255.0 (m/pow (/ idx 512.0) 0.4))]
        (set-color canvas col col col)
        (rect canvas x y 1 1))))
  make-matrix)

(defn draw-julia
  "Draw Julia type"
  [canvas make-matrix c]
  (dotimes [x hw]
    (dotimes [y hw]
      (let [xx (m/mnorm x 0.0 hw -3.0 3.0)
            yy (m/mnorm y 0.0 hw -3.0 3.0)
            sz (make-matrix xx yy)
            ^int idx (loop [iter (int 0)
                            z sz]
                       (if (and (< iter 64)
                                (< (absm z) 4.0))
                         (recur (inc iter) (v/add (mulm z z) c)) 
                         (dec iter)))
            col (* 255.0 (m/pow (/ idx 64.0) 0.4))]
        (set-color canvas col col col)
        (rect canvas x y 1 1))))
  make-matrix)

(defn check-matrix-def
  "Returns true if matrix definition is valid (ie. reversible)"
  [matrix-creator]
  (let [generator (map #(vector %1 %2)
                       (repeatedly #(r/drand -100.0 100.0))
                       (repeatedly #(r/drand -100.0 100.0)))
        not-zerof (filter #(not (v/is-zero? %)))
        matrix-m (map matrix-creator)
        det-m (map det)
        checker (comp not-zerof matrix-m det-m (take 100000))]
    (not-every? clojure.core/zero? (transduce checker conj generator))))

(defn make-matrix-maker
  "Create matrix maker. Values are result of applying some random functions or their combinations"
  []
  (let [funs [(fn ^double [^double a _] a)
              (fn ^double [_ ^double b] b)
              m/fast+
              m/fast*
              clojure.core/- #(m/atan2 %1 %2) #(m/hypot %1 %2)
              (fn ^double [a _] (m/sin a))
              (fn ^double [_ b] (m/sin b))
              (fn ^double [a b] (r/noise a b))
              (fn ^double [^double a ^double b] (if (zero? b) (/ a m/EPSILON) (/ a b)))
              (fn ^double [^double a _] (- a))
              (fn ^double [_ ^double b] (- b))]
        rand-fn (repeatedly #(rand-nth funs))
        newfuns (concat funs (take 5 (map #(let [nf (first rand-fn)]
                                             (fn [a b]
                                               (nf (%1 a b) (%2 a b)))) rand-fn rand-fn)))
        [f1 f2 f3 f4] (repeatedly 4 #(rand-nth newfuns))
        make-matrix-fn (fn make-matrix 
                         ([a b]
                          (Vec4. (f1 a b) (f2 a b) (f3 a b) (f4 a b)))
                         ([[a b]]
                          (make-matrix a b)))]
    (if (check-matrix-def make-matrix-fn) ;; recur if given matrix is invalid.
      make-matrix-fn
      (recur))))

(defn complex-matrix
  "True Complex representation"
  ([a ^double b]
   (Vec4. a (- b) b a))
  ([[a b]]
   (complex-matrix a b)))

;; window, context and events

(def mcanvas (canvas w w :low))
(def mwindow (show-window {:canvas mcanvas
                           :window-name "Mandelbrot"
                           :state complex-matrix}))

(def jcanvas (canvas hw hw :low))
(def jwindow (show-window jcanvas "Julia"))

(with-canvas-> mcanvas
  (draw-mandelbrot (get-state mwindow)))

(defmethod key-pressed ["Mandelbrot" \space] [_ _]
  (with-canvas-> mcanvas
    (draw-mandelbrot (make-matrix-maker))))

(defmethod key-pressed ["Mandelbrot" \m] [_ _]
  (with-canvas-> mcanvas
    (draw-mandelbrot complex-matrix)))

(defmethod mouse-event ["Mandelbrot" :mouse-moved] [e matrix-maker]
  (let [nx (m/mnorm (mouse-y e) 0.0 w -3.0 3.0)
        ny (m/mnorm (mouse-x e) 0.0 w -3.0 3.0)]
    (with-canvas-> jcanvas
      (draw-julia matrix-maker (matrix-maker nx ny)))))

(defmethod key-pressed ["Mandelbrot" \s] [_ s]
  (save mcanvas (next-filename "results/ex35/mandelbrot" ".jpg"))
  (save jcanvas (next-filename "results/ex35/julia" ".jpg"))
  s)

