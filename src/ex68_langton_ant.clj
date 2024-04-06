(ns ex68-langton-ant
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]))

(defn add [[x1 y1] [x2 y2] w h] [(mod (+ x1 x2) w) (mod (+ y1 y2) h)])

(defn move [buff [[x y :as pos] [dx dy]]]
  (let [w (c2d/width buff)
        h (c2d/height buff)
        col? (pos? (p/get-value buff 0 x y))
        ndir (if col? [dy (- dx)] [(- dy) dx])]
    (p/set-color! buff x y (if col? :black :white))
    [(add pos ndir w h) ndir]))

(def speed 100)

(defn draw [canvas _ frame [buff [[x y] _ :as pos-dir]]]
  (-> canvas
      (c2d/set-background 20 20 20)
      (c2d/image (c2d/resize buff 800 800))
      (c2d/set-color :blue 200)
      (c2d/rect 10 10 100 35)
      (c2d/set-color :white)
      (c2d/set-font-attributes 20)
      (c2d/text (str (* speed frame)) 20 35)
      (c2d/set-color :red)
      (c2d/set-stroke 2)
      (c2d/ellipse (* 2 x) (* 2 y) 10 10 true ))
  [buff (nth (iterate (partial move buff) pos-dir) speed)])

(def window (c2d/show-window {:canvas (c2d/canvas 800 800)
                            :background :black
                            :draw-fn draw
                            :draw-state [(p/pixels 400 400) [[100 100] [0 1]]]}))
