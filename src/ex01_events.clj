;; ## Example 01
;;
;; Show how to create mouse and key events + replace-canvas function

(ns ex01-events
  "Process window events"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; In this example you can see how to create event methods for different windows
;;
;; Let's create two windows and define following actions:
;;
;; * Presse `space` in both windows to display window name
;; * For released mouse on first window do the following:
;;     * create new canvas
;;     * get mouse position and change to the color
;;     * set canvas color to newly calculated
;;     * replace canvas
;;
;; Note: this is just ilustrations how to replace canvas, simpler is just change color on canvas attached to window (without replacing it).

(defn example-01
  "Create 2 windows and attach event methods"
  []
  (let [name1 "first window"
        name2 "second window"
        frame1 (show-window (canvas 1 1) name1 400 400 25) ;; first window
        frame2 (show-window (canvas 100 100) name2 400 200 10)] ;; second window
    
    (defmethod key-pressed [name1 \space] [_ _] ;; event on space
      (println (str "Window: " name1)))

    (defmethod key-pressed [name2 \space] [_ _] ;; event on space
      (println (str "Window: " name2)))

    (defmethod mouse-event [name1 :mouse-pressed] [e _] ;; event on mouse for first window
      (let [c (canvas 1 1) ;; create new canvas
            x (mouse-x e) ;; get mouse x position
            y (mouse-y e) ;; get mouse y position
            cr (m/cnorm x 0 399 0 255) ;; map to color range
            cg (m/cnorm y 0 399 0 255)] ;; map to color range
        (with-canvas-> c
          (set-background cr cg 128)) ;; set color based on mouse position
        (replace-canvas frame1 c))) ;; replace old canvas with new one
    nil))

(example-01)

;; In-loop events

(defn draw-fn
  "Draw events on screen"
  [c w _ _]
  (if (mouse-pressed? w)
    (set-color c :white)
    (set-color c :darkgray))
  (rect c 100 100 100 100)
  (if (key-pressed? w)
    (do
      (set-color c :white)
      (when (= :space (key-code w)) (println @(:events w))))
    (set-color c :darkgray))
  (rect c 300 100 100 100)
  (set-color c :gray)
  (text c "Mouse" 110 120 )
  (text c "Key" 310 120))

;; click and press keys to light squares
;; press SPACE to display all event data

(def window (show-window {:canvas (canvas 500 300)
                          :draw-fn draw-fn
                          :fps 30
                          :window-name "In-loop events"}))

