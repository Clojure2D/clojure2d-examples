(ns ex66-spinning-flower-mandala
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [clojure2d.extra.overlays :as o]
            [fastmath.core :as m]))

;; make things as fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw [canvas window ^long frameno
            {^long start-time :start-time
             ^long outer-radius :outer-radius
             :as state}]
  (let [curr-time (. System (nanoTime))
        elapsed-time (m/- curr-time start-time)
        frame-rate (m// (m/* frameno 1000000000.0) elapsed-time)
        o-r (m/* 2 (m/+ outer-radius (m/* 5 (m/sin (m// frameno m/TWO_PI 5)))))
        rot-angle (m/mod (m// frameno m/TWO_PI 60) m/TWO_PI)]
    ;;Translate and setup coords
    (-> canvas
        (c2d/scale 2)
        (c2d/translate 150 150)
        (c2d/rotate rot-angle)
        (c2d/flip-y))
    ;;Back halo
    (-> canvas
        (c2d/set-color 116 88 20 120)
        (c2d/ellipse 0 0 o-r o-r)
        (c2d/set-color 150 100 30 120)
        (c2d/ellipse 0 0 (m/- o-r 2) (m/- o-r 2))
        (c2d/set-color 0 0 0 120)
        (c2d/ellipse 0 0 (m/- o-r 8) (m/- o-r 8))
        (c2d/set-color 232 177 40 120)
        (c2d/ellipse 0 0 (m/- o-r 8) (m/- o-r 8)))
    ;;Blur halo
    (p/set-canvas-pixels! canvas
     (->> canvas
             p/to-pixels
             (p/filter-channels p/gaussian-blur-5)))
    ;;Idda flower at the center https://en.wikipedia.org/wiki/Wrightia_antidysenterica
    (dotimes [_ 5]
      (c2d/set-color canvas
                     240 240 240
                     (abs (m/* 180 (m/sin (m/* 1.5 rot-angle)))))
      (c2d/shape canvas (:idda-petal state))
      (c2d/set-color canvas 240 180 40 (abs (m/* 180 (m/sin (m/* 1.5 rot-angle)))))
      ;(c2d/ellipse canvas 0 0 16 16)
      (c2d/rotate canvas (m// m/TWO_PI 5)))
    ;;Blur it a bit
    (p/set-canvas-pixels! canvas
     (->> canvas
          p/to-pixels
          (p/filter-channels p/gaussian-blur-2)))
    ;;Rotate lotus other way
    (c2d/rotate canvas (m/- m/TWO_PI))
    (c2d/rotate canvas (m/* 4 rot-angle))
    ;;Outer red lotus petals
    (dotimes [_ 6]
      (c2d/set-color canvas (m/+ 90 (m/* (m/sin (m// frameno m/TWO_PI 60)) 40)) 0 0 220)
      (c2d/shape canvas (:lotus-petal state))
      (c2d/rotate canvas (m// m/TWO_PI 6)))
    ;;Blur a bit
    (p/set-canvas-pixels! canvas
         (->> canvas
              p/to-pixels
              (p/filter-channels (p/gaussian-blur 1))))
    ;;Reset all translations
    (c2d/reset-matrix canvas)
    ;;noise
    (-> canvas
      (c2d/image ((:noises state) (mod frameno 20))))
    ;;Framerate display
    (-> canvas
        (c2d/set-color 0 0 0)
        (c2d/set-stroke 1)
        (c2d/text (str frame-rate) 20 20 :left))
    ;;Title
    #_(-> canvas
          (c2d/scale 2)
          (c2d/set-color 180 160 80 220)
          (c2d/text "Mandala" 40 40 :center))
    ;;Output images
    #_(when (:to-video? state)
        (c2d/save canvas (str "out/petals" frameno ".jpg"))
        (when (> frameno 100)
          (c2d/close-window window)))
        ;(shell/with-sh-dir "out"
        ;  (shell/sh "ffmpeg" "-y" "-i" "petals%d.jpg" "-c:v" "libx264" "-pix_fmt" "yuv420p" "-r" "60" "output.mp4")
        ;  (Thread/sleep 5000))))
    state))

(def window
  (c2d/show-window {:canvas (c2d/canvas 600 600 :high)
                    :window-name "mandala"
                    :hint :mid
                    :always-on-top? true
                    :position [0 0]
                    :draw-fn #'draw
                    :fps 60
                    :setup (fn [c _]
                             (c2d/set-background c 45 45 40)
                             {:start-time (. System (nanoTime))
                              :to-video? true
                              :outer-radius 116
                              :noises (vec (repeatedly 20 #(o/noise-overlay 300 300 {:alpha 25})))
                              :spots (o/spots-overlay 50 50 {:alpha 100 :intensities [10 20 30 40 50]})
                              :lotus-petal (c2d/path-def->shape
                                                 [[:move [0 50]]
                                                  [:cubic [-8 50 -16 44 -25 44]]
                                                  [:line [-50 87]]
                                                  [:cubic [-55 110 0 100 0 120]]
                                                  [:cubic [0 100 55 110 50 87]]
                                                  [:line [25 44]]
                                                  [:cubic [16 44 8 50 0 50]]
                                                  [:close]])
                              :idda-petal (c2d/path-def->shape
                                              [[:move [0 0]]
                                               [:cubic [-16 20 -35 45 0 50]]
                                               [:cubic [35 45 16 20 0 0]]])})}))
