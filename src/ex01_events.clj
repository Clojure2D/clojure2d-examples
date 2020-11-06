;; ## Example 01
;;
;; Show how to create mouse and key events + replace-canvas function

(ns ex01-events
  "Process window events"
  (:require [clojure2d.core :as c2d]
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
        frame1 (c2d/show-window (c2d/canvas 1 1) name1 400 400 25) ;; first window
        _ (c2d/show-window (c2d/canvas 100 100) name2 400 200 10)] ;; second window
    
    (defmethod c2d/key-pressed [name1 \space] [_ _] ;; event on space
      (println (str "Window: " name1)))

    (defmethod c2d/key-pressed [name2 \space] [_ _] ;; event on space
      (println (str "Window: " name2)))

    (defmethod c2d/mouse-event [name1 :mouse-pressed] [e _] ;; event on mouse for first window
      (let [c (c2d/canvas 1 1) ;; create new canvas
            x (c2d/mouse-x e) ;; get mouse x position
            y (c2d/mouse-y e) ;; get mouse y position
            cr (m/cnorm x 0 399 0 255) ;; map to color range
            cg (m/cnorm y 0 399 0 255)] ;; map to color range
        (c2d/with-canvas-> c
          (c2d/set-background cr cg 128)) ;; set color based on mouse position
        (c2d/replace-canvas frame1 c))) ;; replace old canvas with new one
    nil))

(example-01)

;; In-loop events

(defn draw-fn
  "Draw events on screen"
  [c w _ _]
  (if (c2d/mouse-pressed? w)
    (c2d/set-color c :white)
    (c2d/set-color c :darkgray))
  (c2d/rect c 100 100 100 100)
  (if (c2d/key-pressed? w)
    (do
      (c2d/set-color c :white)
      (when (= :space (c2d/key-code w)) (println @(:events w))))
    (c2d/set-color c :darkgray))
  (c2d/rect c 300 100 100 100)
  (c2d/set-color c :black)
  (c2d/text c "Mouse" 110 120 )
  (c2d/text c "Key" 310 120))

;; click and press keys to light squares
;; press SPACE to display all event data

(def window (c2d/show-window {:canvas (c2d/canvas 500 300)
                              :draw-fn draw-fn
                              :fps 30
                              :window-name "In-loop events"}))

