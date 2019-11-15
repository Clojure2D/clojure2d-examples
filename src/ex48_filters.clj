;; reconstruction filters visualised

(ns examples.ex48-filters
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.core :as m])
  (:import [clojure2d.java.reconstruction AFilter Gaussian Hann CosineBell Triangle BlackmanHarris Mitchell Sinc Box]))

(def ^:const ^int size 15)
(def ^:const ^int hsize (/ size 2))
(def stroke (c/gray 20))

(defn kernel
  ""
  [tp radius spread]
  (condp = tp
    :gaussian (Gaussian. radius spread)
    :hann (Hann. radius spread)
    :cosinebell (CosineBell. radius spread)
    :triangle (Triangle. radius spread)
    :box (Box. radius)
    :sinc (Sinc. radius spread)
    :mitchell (Mitchell. radius spread m/THIRD m/THIRD)
    :cubic (Mitchell. radius spread 1.0 0.0)
    :catmull-rom (Mitchell. radius spread 0.0 0.5)
    :blackman-harris (BlackmanHarris. radius spread)
    nil))

(defn draw
  ""
  [canvas window _ _]
  (let [vx (m/approx (* 0.05 (int (m/norm (mouse-x window) 0 600 0.0001 200))))
        vy (m/approx (* 0.05 (int (m/norm (mouse-y window) 0 600 0.0001 100))))
        ^AFilter filter (kernel (get-state window) vx vy)]
    (-> canvas
        (set-background (c/gray 15))
        (set-color :white)
        (text (.getName filter) 10 12)
        (text (str "Radius: " vx) 10 26)
        (text (str "Spread: " vy) 10 40)
        (translate 300 300))
    
    (dotimes [x 16]
      (dotimes [y 16]
        (let [fv (* 256.0 (aget (.-filterTable filter) (+ x (* y 16))))
              xsize (* size x)
              ysize (* size y)
              col (if (neg? fv) (c/color (m/abs fv) 0 0) (c/gray fv))]
          (-> canvas
              (filled-with-stroke col stroke crect xsize ysize size size)
              (filled-with-stroke col stroke crect (- xsize) ysize size size)
              (filled-with-stroke col stroke crect xsize (- ysize) size size)
              (filled-with-stroke col stroke crect (- xsize) (- ysize) size size)))))
    
    (let [rad (+ 2 (int (m/floor (- (.-radius filter) 0.5))))]
      (dotimes [x rad]
        (let [xx (min (int (m/floor (m/abs (* x (.-iradius16 filter))))) 15)
              xsize (* xx size)]
          (-> canvas
              (set-color (c/gray 200 50))
              (line xsize 250 xsize 500)
              (line (- xsize) 250 (- xsize) 500))
          (dotimes [y rad]
            (let [yy (min (int (m/floor (m/abs (* y (.-iradius16 filter))))) 15)
                  ysize (* yy size)]
              (-> canvas
                  (filled-with-stroke :green stroke crect xsize ysize hsize hsize)
                  (filled-with-stroke :green stroke crect (- xsize) ysize hsize hsize)
                  (filled-with-stroke :green stroke crect xsize (- ysize) hsize hsize)
                  (filled-with-stroke :green stroke crect (- xsize) (- ysize) hsize hsize)))))))

    (-> canvas
        (set-color 100 100 255)
        (line -300 400 300 400))

    (let [l (* -16 size)
          r (* 16 size)]
      (-> canvas
          (set-color :white 200)
          (path (for [x (range l r 4)
                      :let [xx (m/norm x l r (- (.-radius filter)) (.-radius filter))
                            yy (* 100.0 (.evaluate filter xx 0))]]
                  [x (- 400 yy)])))))
  filter)

(def window (show-window {:window-name "Kernels"
                          :canvas (canvas 600 800)
                          :draw-fn draw
                          :state :gaussian}))

(defmethod key-pressed ["Kernels" \1] [_ _] :gaussian)
(defmethod key-pressed ["Kernels" \2] [_ _] :hann)
(defmethod key-pressed ["Kernels" \3] [_ _] :cosinebell)
(defmethod key-pressed ["Kernels" \4] [_ _] :triangle)
(defmethod key-pressed ["Kernels" \5] [_ _] :box)
(defmethod key-pressed ["Kernels" \6] [_ _] :sinc)
(defmethod key-pressed ["Kernels" \7] [_ _] :mitchell)
(defmethod key-pressed ["Kernels" \8] [_ _] :cubic)
(defmethod key-pressed ["Kernels" \9] [_ _] :catmull-rom)
(defmethod key-pressed ["Kernels" \0] [_ _] :blackman-harris)
