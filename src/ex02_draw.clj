(ns ex02-draw
  "Draw callback function" 
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]))

;; make things as fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw frame"
  [canvas _ ^long frameno _] ;; callback function accepts canvas, window, frame number and current state
  (let [fc (/ frameno 100.0) ;; scale frame number down
        n (->> fc
               (m/tan)
               (m/qsin)
               (m/abs)
               (+ 0.1)) ;; math function responsible for object shape
        ^double cn (m/cnorm n 0.1 1.0 0.0 20) ;; normalize 
        ew (* n 160.0) ;; width of ellipse
        eh (* (- 1.0 n) 160.0)] ;; height of ellipse

    (-> canvas
        (c2d/set-color 45 45 41 20) ;; set color for background
        (c2d/rect 0 0 (c2d/width canvas) (c2d/height canvas)) ;; draw background with alpha, to fake motion blur
        (p/set-canvas-pixels! (->> canvas ;; take canvas
                                   p/to-pixels ;; convert to pixels
                                   (p/filter-channels p/gaussian-blur-2))) ;; operate on pixels directly - blur three channels (skip alpha)

        (c2d/set-color (- 146.0 ew) (- 199.0 cn) (- 163.0 eh)) ;; set color
        (c2d/ellipse 100 100 ew eh)))) ;; draw ellipse

(def window (c2d/show-window {:canvas (c2d/canvas 200 200 :mid) ;; create canvas with mid quality
                              :window-name "ellipse"             ;; name window
                              :w 400 ;; size of window (twice as canvas)
                              :h 400
                              :hint :mid ;; hint for drawing canvas on window, mid quality (affects scalling 200 -> 400)
                              :draw-fn draw
                              :setup (fn [c _]
                                       (c2d/set-background c 45 45 41))})) ;; draw callback funtion


;; save to the file, it saves attached canvas not resized window content itself
(comment save window "results/ex02/blob.jpg")

;; [[../results/ex02/blob.jpg]]
