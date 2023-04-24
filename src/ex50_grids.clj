(ns ex50-grids
  "Visualize grids

  Move mouse around and press:

  * 1-6 - to change grid type
  * up/down - to change cell size"
  (:require [clojure2d.core :as c2d]
            [fastmath.grid :as grid]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

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
  (let [g (:grid (c2d/get-state window))
        m (c2d/mouse-pos window)
        off (v/vec2 (/ (c2d/width canvas) 2) (/ (c2d/height canvas) 2))
        moff (v/sub m off)
        cell (grid/coords->cell g moff)
        anchor (grid/cell->anchor g cell)
        mid (grid/coords->mid g moff)]
    (when (and (pos? (int (m 0))) (pos? (int (m 1))))
      (-> canvas
          (c2d/push-matrix)
          (c2d/translate off)
          (c2d/set-background :black 50)
          (c2d/filled-with-stroke :white :maroon c2d/grid-cell g (moff 0) (moff 1) 1.0)
          (c2d/set-color :green)
          (c2d/ellipse (anchor 0) (anchor 1) 8 8 true)
          (c2d/set-color :red)
          (c2d/ellipse (mid 0) (mid 1) 5 5)
          (c2d/pop-matrix)
          (c2d/set-color :black)
          (c2d/rect 10 10 140 40)
          (c2d/set-color :white)
          (c2d/text g 15 22)
          (c2d/text cell 15 36)))))

(c2d/show-window {:canvas (c2d/canvas 1000 800 :highest)
                  :window-name "Grids"
                  :draw-fn draw
                  :state (state-data :flat-hex 100.0)})

(defmethod c2d/key-pressed ["Grids" \1] [_ s]  (state-data :square (:size s)))
(defmethod c2d/key-pressed ["Grids" \2] [_ s]  (state-data :shifted-square (:size s)))
(defmethod c2d/key-pressed ["Grids" \3] [_ s]  (state-data :rhombus (:size s)))
(defmethod c2d/key-pressed ["Grids" \4] [_ s]  (state-data :triangle (:size s)))
(defmethod c2d/key-pressed ["Grids" \5] [_ s]  (state-data :flat-hex (:size s)))
(defmethod c2d/key-pressed ["Grids" \6] [_ s]  (state-data :pointy-hex (:size s)))

(defmethod c2d/key-pressed ["Grids" c2d/virtual-key] [e s]
  (let [^double size (:size s)
        t (grid/grid-type (:grid s))]
    (case (c2d/key-code e)
      :up (state-data t (inc size))
      :down (state-data t (if (<= size 3) size (dec size)))
      s)))


