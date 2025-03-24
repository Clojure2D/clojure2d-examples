(ns neuroevolution.core
  (:require [neuroevolution.draw :as draw]
            [neuroevolution.environment :as env]
            [neuroevolution.simulate :as sim]
            [clojure2d.core :as c2d]))

(def ^:const WINDOW-SIZE 800)

(def environment-config
  {:time-limit 10.0 ;; how much seconds to collect a dot
   :time-mult 3.0   ;; time penalty multiplier
   :dots-count 10   ;; dots to collect
   :rot-mult 0.2    ;; rotation penalty multiplier
   })

(def car-config
  {:max-velocity 15.0 ;; max speed
   :max-acceleration-f 1.5 ;; max acc forward
   :max-acceleration-b 0.7 ;; max acc backward
   :max-rotation 0.05 ;; max rotation speed in radians
   :friction-factor 0.95 ;; amount of speed value kept each frame (1.0 - no friction, 0.9 - high friction)
   })

(def simulation-config
  {:envrionments-count 1 ;; how many distinct environments use to train
   :profile [3 5 5 2] ;; network profile: 3 inputs, 2 outputs + hidden layers (4,4)
   :population-size 600 ;; how many neural networks should play the game
   :elite-percent 0.1 ;; how many best (percent)  neural networks should be used in the next gen
   :environment-config environment-config ;; default environment
   :car-config car-config ;; default car
   })

(defn draw-frame
  [canvas window _ env]
  (draw/draw canvas env)
  (if (:game-over? env)
    env
    (draw/update-env env (c2d/get-state window))))


(defmethod c2d/key-event ["Car" :key-pressed] [event state]
  (conj state (c2d/key-char event)))

(defmethod c2d/key-event ["Car" :key-released] [event state]
  (disj state (c2d/key-char event)))

(defn play-game
  "Play game; j/l - left/right; i/k - accelerated/decelerate"
  ([] (play-game {} {}))
  ([environment-config car-config]
   (c2d/show-window {:canvas (c2d/canvas WINDOW-SIZE WINDOW-SIZE :high "Andale Mono")
                     :window-name "Car"
                     :draw-fn draw-frame
                     :draw-state (env/environment WINDOW-SIZE environment-config car-config)
                     :state #{}})))

(comment (play-game))

;; simulation

(comment
  ;; run 50 first generation on new environments
  (def sim1 (sim/run-all-games-n (sim/init-state simulation-config) 50))
  ;; run 50 more generations
  (def sim1 (sim/run-all-games-n sim1 50))
  ;; run 50 more generations on new envrionments (fix overfitting)
  (def sim1 (sim/run-all-games-n sim1 50 true)))

(defn draw-sim-frame
  [canvas _ _ [env profile chr :as state]]
  (draw/draw canvas env)
  (if (:game-over? env)
    state
    [(sim/step env profile chr) profile chr]))

(def some-env (env/environment WINDOW-SIZE environment-config car-config))

(defn nn-play
  ([simulation-data] (nn-play simulation-data nil))
  ([simulation-data environment] (nn-play simulation-data environment 0))
  ([simulation-data environment car-id]
   (c2d/show-window {:canvas (c2d/canvas WINDOW-SIZE WINDOW-SIZE :high "Andale Mono")
                     :window-name "Car - chromosome"
                     :draw-fn draw-sim-frame
                     :draw-state [(or environment (rand-nth (:environments simulation-data)))
                                  (:profile-eval simulation-data)
                                  (nth (:population simulation-data) car-id)]
                     :state #{}})))

(comment
  ;; visualize the best car
  (nn-play sim1)

  ;; visualize the worst car
  (nn-play sim1 nil (dec (:population-size sim1)))

  ;; visualize the best car on fresh environement
  (nn-play sim1 some-env)
  )
