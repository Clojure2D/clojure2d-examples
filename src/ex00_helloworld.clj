;; Complete minimal example

(ns ex00-helloworld
  "Complete minimal example"
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.core :as m]))

;; be sure everything is fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; define canvas
(def my-canvas (canvas 600 600))

;; create window
(def window (show-window my-canvas "Hello World!"))

;; draw rectangle with line wrapping with threading canvas context
(with-canvas-> my-canvas ;; prepare drawing context in canvas
  (set-background 10 5 5) ;; clear background
  (set-color 210 210 200) ;; set color
  (rect 100 100 400 400) ;; draw rectangle
  (set-color 50 50 60) ;; set another color
  (set-stroke 2.0) ;; set line width
  (line 50 300 550 300) ;; draw line
  (set-font-attributes 30) ;; set font size
  (set-color :maroon) ;; set current color
  (text "Hello World!" 110 130)) ;; draw line

;; draw dots
(with-canvas [c my-canvas]
  (set-color c :black) ;; set color to black
  (doseq [angle (range 0.0 m/TWO_PI (/ m/TWO_PI 10.0))] ;; draw circles around mid of canvas
    (ellipse c
             (+ 300.0 (* 30.0 (m/sin angle)))
             (+ 300.0 (* 30.0 (m/cos angle)))
             10 10)))

;; saving, save small version of canvas
(comment save (resize my-canvas 300 300) "results/ex00/helloworld.jpg")

;; [[../results/ex00/helloworld.jpg]]

;; now lets define drawing function
;; type hints to avoid boxed operations
(defn draw
  "Draw rotating rectangle. This function is prepared to be run in refreshing thread from your window."
  [canvas ;; canvas to draw on
   window ;; window bound to function (for mouse movements)
   ^long framecount ;; frame number
   state] ;; state (if any), not used here
  (let [midwidth (* 0.5 ^long (width canvas))] ;; find middle of the canvas

    (-> canvas ;; use canvas (context is already ready! It's draw function.)
        (set-background :linen) ;; clear background with :inen color
        (translate midwidth midwidth) ;; set origin in the middle
        (rotate (/ framecount 100.0)) ;; rotate clockwise (based on number of frame)
        (set-color :maroon) ;; set color to maroon
        (crect 0 0 midwidth midwidth) ;; draw centered rectangle
        (rotate (/ framecount -90.0)) ;; rotate counterclockwise
        (set-color 255 69 0 200) ;; set color to orange with transparency
        (crect 0 0 (* 0.9 midwidth) (* 0.9 midwidth))))) ;; draw smaller rectangle

;; run twice!
(def window (show-window (canvas 600 600) "Rotating square" draw)) ;; create canvas, display window and draw on canvas via draw function (60 fps)

;; save on space pressed (be aware that saving is not synchronized with drawing. Occassional glitches may appear.
(defmethod key-pressed ["Rotating square" \space] [_ _] 
  (save (resize (get-image window) 300 300) "results/ex00/rotating.jpg"))

;; [[../results/ex00/rotating.jpg]]
