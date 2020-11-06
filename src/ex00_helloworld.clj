(ns ex00-helloworld
  "Complete minimal example"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

;; be sure everything is fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; define canvas
(def my-canvas (c2d/canvas 600 600))

;; create window
(def window (c2d/show-window my-canvas "Hello World!"))

;; draw rectangle with line wrapping with threading canvas context
(c2d/with-canvas-> my-canvas ;; prepare drawing context in canvas
  (c2d/set-background 10 5 5) ;; clear background
  (c2d/set-color 210 210 200) ;; set color
  (c2d/rect 100 100 400 400) ;; draw rectangle
  (c2d/set-color 50 50 60) ;; set another color
  (c2d/set-stroke 2.0) ;; set line width
  (c2d/line 50 300 550 300) ;; draw line
  (c2d/set-font-attributes 30) ;; set font size
  (c2d/set-color :maroon) ;; set current color
  (c2d/text "Hello World!" 110 130)) ;; draw line

;; draw dots
(c2d/with-canvas [c my-canvas]
  (c2d/set-color c :black) ;; set color to black
  (doseq [angle (range 0.0 m/TWO_PI (/ m/TWO_PI 10.0))] ;; draw circles around mid of canvas
    (c2d/ellipse c
                 (+ 300.0 (* 30.0 (m/sin angle)))
                 (+ 300.0 (* 30.0 (m/cos angle)))
                 10 10)))

;; saving, save small version of canvas
#_(c2d/save (c2d/resize my-canvas 300 300) "results/ex00/helloworld.jpg")

;; [[../results/ex00/helloworld.jpg]]

;; now lets define drawing function
;; type hints to avoid boxed operations
(defn draw
  "Draw rotating rectangle. This function is prepared to be run in refreshing thread from your window."
  [canvas ;; canvas to draw on
   _ ;; window bound to function (for mouse movements)
   ^long framecount ;; frame number
   _] ;; state (if any), not used here
  (let [midwidth (* 0.5 ^long (c2d/width canvas))] ;; find middle of the canvas

    (-> canvas ;; use canvas (context is already ready! It's draw function.)
        (c2d/set-background :linen) ;; clear background with :inen color
        (c2d/translate midwidth midwidth) ;; set origin in the middle
        (c2d/rotate (/ framecount 100.0)) ;; rotate clockwise (based on number of frame)
        (c2d/set-color :maroon) ;; set color to maroon
        (c2d/crect 0 0 midwidth midwidth) ;; draw centered rectangle
        (c2d/rotate (/ framecount -90.0)) ;; rotate counterclockwise
        (c2d/set-color 255 69 0 200) ;; set color to orange with transparency
        (c2d/crect 0 0 (* 0.9 midwidth) (* 0.9 midwidth))))) ;; draw smaller rectangle

;; run twice!
(def window2 (c2d/show-window (c2d/canvas 600 600) "Rotating square" draw)) ;; create canvas, display window and draw on canvas via draw function (60 fps)

;; save on space pressed (be aware that saving is not synchronized with drawing. Occassional glitches may appear.
(defmethod c2d/key-pressed ["Rotating square" \space] [_ _] 
  (c2d/save (c2d/resize (c2d/get-image window2) 300 300) "results/ex00/rotating.jpg"))

;; [[../results/ex00/rotating.jpg]]
