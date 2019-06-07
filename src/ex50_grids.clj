(ns ex50-grids
  "Visualize grids

  Move mouse around and press:

  * 1-6 - to change grid type
  * up/down - to change cell size"
  (:require [clojure2d.core :refer :all]
            [fastmath.grid :as grid]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- state-data 
  "Create new grid"
  [type ^double size]
  {:grid (grid/grid type size)
   :size size})

(defn draw
  "Draw grid"
  [canvas window _ _]
  (let [g (:grid (get-state window))
        m (mouse-pos window)
        cell (grid/coords->cell g m)
        anchor (grid/cell->anchor g cell)
        mid (grid/coords->mid g m)]
    (when (and (pos? (int (m 0))) (pos? (int (m 1))))
      (-> canvas
          (set-background :black 50)
          (filled-with-stroke :white :maroon grid-cell g (m 0) (m 1) 1.0)
          (set-color :green)
          (ellipse (anchor 0) (anchor 1) 5 5)
          (ellipse (mid 0) (mid 1) 5 5)
          (set-color :black)
          (rect 10 10 140 40)
          (set-color :white)
          (text g 15 22)
          (text cell 15 36)))))

(show-window {:canvas (canvas 1000 800 :highest)
              :window-name "Grids"
              :draw-fn draw
              :state (state-data :flat-hex 30.0)})

(defmethod key-pressed ["Grids" \1] [_ s]  (state-data :square (:size s)))
(defmethod key-pressed ["Grids" \2] [_ s]  (state-data :shifted-square (:size s)))
(defmethod key-pressed ["Grids" \3] [_ s]  (state-data :rhombus (:size s)))
(defmethod key-pressed ["Grids" \4] [_ s]  (state-data :triangle (:size s)))
(defmethod key-pressed ["Grids" \5] [_ s]  (state-data :flat-hex (:size s)))
(defmethod key-pressed ["Grids" \6] [_ s]  (state-data :pointy-hex (:size s)))

(defmethod key-pressed ["Grids" virtual-key] [e s]
  (let [^double size (:size s)
        t (grid/grid-type (:grid s))]
    (case (key-code e)
      :up (state-data t (inc size))
      :down (state-data t (if (<= size 3) size (dec size)))
      s)))

