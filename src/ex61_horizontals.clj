(ns ex61-horizontals
  (:require [fastmath.fields :as f]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.extra.utils :as u]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure2d.extra.overlays :as o]))

(def no (o/noise-overlay 600 600))

(def res (binding [f/*skip-random-fields* (r/brand 0.9)]
         (let [b1 (r/drand -12.0 -3.0) ;; left vector field boundary
               b2 (- b1) ;; right boundary
               field (f/random-field) ;; random vector field
               #_#_pal (c/palette)  
               #_#_pal [:black (c/gray 100) :white (c/gray 127.5) :black (c/gray 155) :white]
               pal [:black (c/gray 100) (c/gray 155) :white] ;; some palette
               palcnt (dec (count pal))] ;; palette count
           (c2d/with-canvas [c (c2d/canvas 600 600)]
             (c2d/set-background c (c/gray 240))
             (doseq [y (range 600) ;; traverse vertically, line by line
                     :let [yy (m/norm y 0.0 600.0 b1 b2) ;; screen to field coords
                           ;; calculate line width (horizontal step) as some value based on field vector (at the left boundary) magnitude
                           step (-> (v/vec2 b1 yy) field v/mag m/sin m/sq inc (* 0.5))]]
               (loop [xx (double b1)] ;; traverse horizontally, step-size
                 (when (< xx b2)
                   (let [fv (field (v/vec2 xx yy)) ;; vector field value
                         nxx (+ xx step) ;; line segment end
                         x (m/norm xx b1 b2 0.0 600.0) ;; field to screen coords
                         nx (m/norm nxx b1 b2 0.0 600.0)
                         h (m/sin (v/heading fv)) ;; v/mag ;; angle of the field vector...
                         pv (m/round (m/norm h -1.0 1.0 0.0 palcnt))] ;; ... is mapped to the palette
                     (c2d/set-color c (pal pv)) ;; set color
                     (c2d/line c x y nx y) ;; draw line segment
                     (recur nxx)))))
             (let [img (o/render-noise ;; apply noise overlay
                        (c2d/to-image c) no)]
               (u/show-image img)
               img)))))

(comment
  (c2d/save res (c2d/next-filename "results/ex61/" ".jpg"))
  )
