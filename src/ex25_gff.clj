;; http://math.mit.edu/~sheffield/spokes.html
;;
;; Animate kappa value from `0` to `inf`

(ns ex25-gff
  "Gaussian Free Field" 
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; size of discrete gaussian free field grid (grid of size gff-size x gff-size)
(def ^:const ^int gff-size (r/irand 5 100))
(def ^:const ^int rays 75)
(def ^:const ^double rsteps (/ m/TWO_PI rays))

(defn gff-rows [] (vec (repeatedly gff-size r/grand)))
(def gff (vec (repeatedly gff-size gff-rows)))

(def ^:const ^int w 800)
(def ^:const ^int h 800)

(defn get-field-value
  "Get bilineary interpolated gaussian field value"
  ^double [^double x ^double y]
  (let [px (m/norm x 0 w 0 gff-size)
        py (m/norm y 0 h 0 gff-size)
        ix (m/floor px)
        iy (m/floor py)
        rx (- px ix)
        ry (- py iy)
        ix (unchecked-int (mod ix gff-size))
        iy (unchecked-int (mod iy gff-size))
        ix+ (unchecked-int (mod (inc ix) gff-size))
        iy+ (unchecked-int (mod (inc iy) gff-size))
        vy1 (m/lerp (get-in gff [ix iy]) (get-in gff [ix+ iy]) rx)
        vy2 (m/lerp (get-in gff [ix iy+]) (get-in gff [ix+ iy+]) rx)]
    (m/lerp vy1 vy2 ry)))

(defn my-draw
  ""
  [canvas ^double hf]
  (c2d/set-background canvas 21 20 19)
  (dotimes [t rays]
    (let [theta (* t rsteps)]
      (loop [x (* 0.5 w)
             y (* 0.5 h)
             iter (long 0)]
        (when (< iter 350)
          (let [v (* hf (get-field-value x y))
                sx (m/sin (+ v theta))
                sy (m/cos (+ v theta))]
            (c2d/rect canvas x y 0.8 0.8)
            (recur (+ x sx)
                   (+ y sy)
                   (inc iter))))))))

(defn draw
  ""
  [canvas window ^long frame _]
  (let [kappa (* frame 0.0041)
        hf (/ (m/sqrt (* 8.0 (/ kappa m/PI))) 
              (- 4.0 kappa))]
    (when (== frame 200) 
      (comment c2d/save window "results/ex25/gff.jpg"))
    (my-draw canvas hf)))

(do

  (def c (c2d/canvas w h :highest))

  (c2d/with-canvas-> c
    (c2d/set-color 220 220 210 200))

  (def window (c2d/show-window c "GFF" #(draw %1 %2 %3 %4))))
