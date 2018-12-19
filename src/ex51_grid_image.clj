;; display image on grid
;;
;; click to renew

(ns ex51-grids-image
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.grid :as grid]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def img (p/load-pixels "results/test.jpg"))

(def c (canvas (width img) (height img)))

(defn draw-in-color
  [g c col colfn]
  (set-color c col 10)
  (let [x (r/irand (width img))
        y (r/irand (height img))
        b (m/norm (colfn (p/get-color img x y)) 0 255 0.1 1.0)]
    (grid-cell c g x y b false))
  c)

(defn restart! []
  (with-canvas [c c]
    (set-background c :black)
    (let [g (grid/grid (rand-nth grid/cell-names) (r/irand 4 15))]
      (dotimes [i (r/irand 10000 50000)]
        (let [[col colfn] (rand-nth [[:red c/red]
                                     [(c/color 0 255 0) c/green]
                                     [:blue c/blue]])]
          (draw-in-color g c col colfn))))
    ;; postprocess
    (->> (p/to-pixels c)
         (p/filter-channels p/normalize)
         (p/filter-channels (p/brightness-contrast 1.9 1.3))
         (p/set-canvas-pixels! c))))

(def window (show-window {:canvas c
                          :window-name "Image on grid."}))

(restart!)

(defmethod mouse-event ["Image on grid." :mouse-clicked] [_ _] (restart!))
(defmethod key-pressed ["Image on grid." \space] [_ _]
  (save c (next-filename "results/ex51/" ".jpg")))
