(ns games.the-king.the-king
  (:require [games.the-king.gfx :as gfx]
            [games.the-king.map :as map]
            [games.the-king.physics :as ph]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.easings :as easings]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]))

;; actors definition

(def player-init {:position (v/vec2 4 4)  ;; position
                :angle 0.125    ;; heading
                :velocity (v/vec2) ;; velocity
                :mass 0.2
                :radius 2.0})

(def king-init {:position (v/vec2 20 20) ;; position
              :velocity (v/vec2) ;; velocity
              :mass 0.1
              :radius 8.0}) 

;; game state updates

(defn update-happiness
  [state value]
  (update state :happy (fn [happy] (m/constrain (+ happy value) 0.0 120.0))))

(defn change-state
  ([state game-state]
   (-> (case game-state
         :welcome (merge state {:game-over false
                                :snakes #{}
                                :time 0
                                :level 0
                                :happy 120 ;; max HP
                                :hits 0    ;; number of hits by player
                                :next-hits 0 ;; hits toleration before king's anger
                                :anger 0     ;; is king angry
                                :snake-cnt 0 ;; number of touched snakes
                                :beer-cnt 0}) ;; beers
         :bounce-in (let [grid (map/calculate-grid (inc (state :level)))]
                      (-> state
                          (assoc :grid grid
                                 :time 0
                                 :background (map/init-map grid))
                          (update :level inc)))
         :game (-> (assoc state
                          :player player-init
                          :king king-init
                          :message-time -1
                          :next-hits (int (m/floor (+ (state :hits) 40 (r/irand 10))))
                          :snakes #{})
                   (dissoc :bottle)
                   (update-happiness 20))
         :bounce-out (assoc state :time 0)
         :game-over (assoc state :time 0))
       (assoc :game-state game-state))))

(defn init-state [] (change-state nil :welcome))

;; game state dispatch

(defmulti update-game-state :game-state)

(defmethod update-game-state :welcome [{:keys [window time] :as state}]
  (if (and (> time 30)
           (c2d/key-pressed? window)
           (#{\z \x} (c2d/key-char window)))
    (change-state state :bounce-in)
    state))

(defmethod update-game-state :bounce-in [state]
  (if (>= (state :time) 90)
    (change-state state :game)
    state))

(defn king-on-target?
  [{[x y] :position}]
  (and (> x 117) (> y 110)))

(defn game-over?
  [{:keys [^long happy]}]
  (m/not-pos? happy))

(defn message
  "Display message"
  [state s]
  (assoc state
         :message s
         :message-time (+ (state :time) 30)))

(defn king-on-water
  "What happens when king stands in water"
  [state]
  (let [nstate (-> state
                   (update-happiness -0.15)
                   (assoc :next-hits (+ (state :hits) 20)))]
    (if (r/brand 0.01)
      (-> (message nstate "wet!")
          (update-happiness -0.05))
      nstate)))

(defn king-on-mud
  "What happens when king stands in mud. More in mud, happiness is lower."
  [state]
  (-> (message state "mud!")
      (update-happiness (m/norm (state :level) 0.5 100.0 -1.0 -10.0))))

(defn king-on-ground
  "What happens when king stands on the ground. King doesn't like to be hit too much."
  [{:keys [next-hits hits level] :as state}]
  (let [nstate (update-happiness state (m/norm level 1.0 100.0 -0.02 -0.15))]
    (if (< next-hits hits)
      (-> (message nstate "ouch!")
          (assoc :next-hits (+ hits (r/irand 20)))
          (update-happiness (- (+ 2 (r/irand (m/norm level 1.0 100.0 1 20.0))))))
      nstate)))

(defn king-position-actions
  "King is not happy to stay in water and mud."
  [{:keys [king grid] :as state}]
  (let [what (map/elevation grid (v/div (king :position) 8.0))]
    (cond
      (zero? what) (king-on-water state)
      (and (= what 1) (r/brand 0.02)) (king-on-mud state)
      :else (king-on-ground state))))

(defn bottle-actions
  "Show bottle when happiness is getting low. Higher levels - higher bottle probability.
  Bottle is visible minimum 13 and maximum 26 seconds."
  [{:keys [happy bottle next-bottle time] :as state}]
  (if (and (< happy 70)
           (not bottle)
           (r/brand (m/norm happy 70 1 0.001 0.02)))
    (assoc state
           :bottle (v/generate-vec2 #(r/irand 20 100))
           :next-bottle (+ time 400 (r/irand 400)))
    (if (and bottle (< next-bottle time))
      (dissoc state :bottle)
      state)))

(defn maybe-remove-snakes
  "There is a small chance that snake disappears"
  [snakes]
  (reduce (fn [snks snk]
            (if (r/brand 0.00005)
              (disj snks snk)
              snks)) snakes snakes))

(defn snakes-actions
  "Add snakes to mud position randomly, higher level more chance for a snake."
  [{:keys [grid level] :as state}]
  (if (r/brand (m/norm level 1 100 0.0005 0.1))
    (let [snx (r/irand 16)
          sny (r/irand 16)]
      (if (= 1 (map/elevation grid snx sny))
        (update state :snakes conj (v/vec2 (+ 4 (* snx 8))
                                           (+ 4 (* sny 8))))
        state))
    (update state :snakes maybe-remove-snakes)))

(defn process-keyboard
  "left/right - rotate player, up/z/x - push forward, down - backward"
  [{:keys [window player] :as state}]
  (if (c2d/key-pressed? window)
    (condp = (c2d/key-code window)
      :left (update-in state [:player :angle] #(- % 0.05))
      :right (update-in state [:player :angle] #(+ % 0.05))
      :up (assoc state :player (ph/apply-force player 1.5))
      :down (assoc state :player (ph/apply-force player -0.1))
      (if (#{\z \x} (c2d/key-char window))
        (assoc state :player (ph/apply-force player 1.5))
        state))
    state))

(defn apply-physics
  "Apply physics function"
  [state f]
  (-> state
      (update :player f)
      (update :king f)))

(defn king-and-bottle
  "What happens when king reaches beer."
  [{:keys [bottle king] :as state}]
  (if (and bottle
           (< (v/dist bottle (king :position))
              (king :radius)))
    (-> state
        (message "beer!")
        (update :beer-cnt inc)
        (update-happiness (r/irand 10 30))
        (dissoc :bottle))
    state))

(defn maybe-touch-snake
  [{:keys [position radius]} snakes]
  (reduce (fn [[snks _ :as curr] snk]
            (if (< (v/dist snk position) radius)
              (reduced [(disj snks snk) true])
              curr)) [snakes false] snakes))

(defn king-and-snakes
  "What happens when king touches snake."
  [{:keys [snakes king happy] :as state}]
  (let [[nsnakes touched?] (maybe-touch-snake king snakes)]
    (if touched?
      (-> state
          (message "snake!")
          (update :snake-cnt inc)
          (update-happiness (* happy (r/drand -0.3 -0.6)))
          (assoc :snakes nsnakes))
      state)))

(defmethod update-game-state :game [{:keys [grid king] :as state}]
  (cond
    (king-on-target? king) (change-state state :bounce-out)
    (game-over? state) (-> (assoc state :game-over true)
                           (change-state :bounce-out))
    :else (-> state
              (king-position-actions)
              (bottle-actions)
              (snakes-actions)
              (process-keyboard)
              (apply-physics (partial ph/apply-gradient grid 0.1))
              (apply-physics ph/limit-speed)
              (ph/check-collisions)
              (king-and-bottle)
              (king-and-snakes)
              (apply-physics (partial ph/update-actor grid)))))

(defmethod update-game-state :bounce-out [{:keys [time game-over] :as state}]
  (if (> time 60)
    (if game-over
      (change-state state :game-over)
      (change-state state :bounce-in))
    state))

(defmethod update-game-state :game-over [{:keys [window] :as state}]
  (if (and (c2d/key-pressed? window)
           (#{\z \x} (c2d/key-char window)))
    (change-state state :welcome)
    state))

(defn update-state
  [state]
  (-> (update-game-state state)
      (update :time inc)))

;; draw

(defmacro time-step
  "Discretize time"
  [time len cnt]
  `(int (mod (/ ~time ~len) ~cnt)))

(defmulti draw :game-state)

(defn draw-pair
  [canvas time]
  (let [king (gfx/sprite :intro :king (time-step time 20 2))
        player (gfx/sprite :intro :boy (if (< (mod time 15) 5) 0 1))]
    (-> canvas
        (gfx/draw-sprite king 10 10)
        (gfx/draw-sprite player 42 26))))

(defmethod draw :welcome [{:keys [canvas time] :as state}]
  (-> (draw-pair canvas time)
      (c2d/set-font gfx/pico-8-mono-font)
      (c2d/set-font-attributes 20)
      (gfx/draw-text "help your king to refresh" 20 64 6)
      (gfx/draw-text "a little bit." 20 70 6)
      (gfx/draw-text "unfortunately terrain" 20 76 6)
      (gfx/draw-text "is rough and swampy." 20 82 6)
      (gfx/draw-text "don't make  king  angry." 20 90 6)
      (gfx/draw-text "by generateme for fc_jam#2" 20 105 5)
      (gfx/draw-text "press    to start" 20 120 7)
      (c2d/set-font gfx/pico-8-font)
      (c2d/set-font-attributes 24)
      (gfx/draw-text "❎" 44 120 7)
      (gfx/draw-text "♥" 61 90 8)
      (gfx/draw-text "♥" 83 90 8))
  state)

(defmethod draw :bounce-in [{:keys [canvas time level background] :as state}]
  (-> (draw-pair canvas time)
      (c2d/set-font gfx/pico-8-mono-font)
      (c2d/set-font-attributes 20)
      (gfx/draw-text (str "level " level) 20 64 6))

  ;; animate dropping map
  (when (>= time 30)
    (let [position (-> time
                       (m/norm 30.0 91.0 0.0 1.0)
                       (easings/bounce-out)
                       (* gfx/canvas-size)
                       (int)
                       (inc))
          img (c2d/subimage background 0 (- gfx/canvas-size position) gfx/canvas-size position)]
      (c2d/image canvas img 0 0)))
  state)

(def ^:const line-y (* 120 gfx/point-size))

(defn happy->color-id
  [happy]
  (-> happy
      (m/norm 120 0 12 8) ;; happy goes from 120 to 0, map it to colors from 11 to 8
      (m/floor)
      (int)))

(defn target-sprite
  "Target sprite frame"
  [time]
  (->> (time-step (+ 15 time) 30 2)
       (gfx/sprite :target)))

(defn snake-sprite
  "Snake sprite frame"
  [time snake-id]
  (->> (time-step time (+ 10.0 (* 2 snake-id)) 2)
       (gfx/sprite :snake)))

(defn snake-offset
  "Offset of the snake horizontal position"
  [time snake-id]
  (-> (/ time (+ snake-id 60.0))
      (* m/TWO_PI)
      (m/cos)
      (* 1.5)))

(defn bottle-sprite
  "Bottle sprite frame"
  [time]
  (->> (time-step time 4 6)
       (gfx/sprite :bottle)))

(defn bottle-offset
  "Vertical bottle offset"
  [time]
  (-> (/ time 60.0)
      (* m/TWO_PI )
      (m/sin)
      (* 2.0)))

(defn player-sprite
  "Plater sprite, animation (8 frames)"
  [angle]
  (->> (-> (+ 0.3125 angle)
           (mod 1.0)
           (* 8.0)
           (m/floor)
           (int))
       (gfx/sprite :boy)))

(defn king-sprite
  "Two frames of king"
  [time]
  (->> (time-step time 30 2) 
       (gfx/sprite :king)))

(defmethod draw :game [{:keys [canvas background time snakes bottle
                               happy message message-time king player] :as state}]
  ;; draw map
  (c2d/image canvas background 0 0)

  ;; animate target
  (gfx/draw-sprite canvas (target-sprite time) 111 103)

  ;; snakes
  (when snakes
    (doseq [[snake-id [x y]] (map-indexed vector snakes)
            :let [snake-sprite (snake-sprite time snake-id)
                  sx (snake-offset time snake-id)]]
      (gfx/draw-sprite canvas snake-sprite (- x 4 sx) (- y 4))))

  ;; bottle
  (when bottle
    (let [[x y] bottle
          bottle-sprite (bottle-sprite time)
          sy (bottle-offset time)]
      (gfx/draw-sprite canvas bottle-sprite (- x 4) (- y 4 sy))))

  ;; player
  (let [{:keys [position angle]} player
        [x y] position
        player-sprite (player-sprite angle)
        ;; hands position
        rot (v/rotate (v/vec2 3 0) (* m/TWO_PI (+ 0.25 angle)))
        [x+ y+] (v/mult (v/add position rot) gfx/point-size)
        [x- y-] (v/mult (v/sub position rot) gfx/point-size)]
    (gfx/draw-sprite canvas player-sprite (- x 4) (- y 4))
    (c2d/set-color canvas (gfx/pico-8-palette 11))
    ;; draw hands
    (c2d/crect canvas x+ y+ 8 8)
    (c2d/crect canvas x- y- 8 8))
  
  ;; king
  (let [[x y] (king :position)
        king-sprite (king-sprite time)]
    (gfx/draw-sprite canvas king-sprite (- x 8) (- y 8)))
  
  ;; prepare fonts
  (-> canvas
      (c2d/set-font gfx/pico-8-font)
      (c2d/set-font-attributes 24))
  
  ;; message
  (when (> message-time time)
    (gfx/draw-sprite canvas (gfx/sprites :message-box) 2 100)
    (gfx/draw-text canvas message 8 110.5 1))

  ;; status
  
  (c2d/set-color canvas (gfx/pico-8-palette 4))
  (c2d/line canvas 2 line-y (- gfx/canvas-size 2) line-y)

  (let [hc (happy->color-id happy)]
    ;; happy bar
    (c2d/filled-with-stroke canvas
                            (gfx/pico-8-palette hc) :black
                            c2d/rect 32 488 (* happy gfx/point-size 0.98) 16)
    ;; happy heart indicator (+ animation when low happiness)
    (when (or (> happy 30)
              (< (mod time 20) 10))
      (gfx/draw-text canvas "♥" 0 126.5 hc)))
  
  state)

(defmethod draw :bounce-out [{:keys [canvas time background] :as state}]
  ;; animate map up
  (let [position (-> time
                     (m/norm 0.0 61.0 0.0 1.0)
                     (easings/circle-in)
                     (* gfx/canvas-size)
                     (int))
        img (c2d/subimage background 0 0 gfx/canvas-size (max 1 (- gfx/canvas-size position)))]
    (c2d/image canvas img 0 position))
  state)

(defmethod draw :game-over [{:keys [canvas time level beer-cnt hits snake-cnt] :as state}]
  (-> (draw-pair canvas time)
      (c2d/set-font gfx/pico-8-mono-font)
      (c2d/set-font-attributes 20)
      (gfx/draw-text "king is angry! game over!" 20 64 6)
      (gfx/draw-text (str "levels " level) 20 80 13)
      (gfx/draw-text (str "beers " beer-cnt) 20 86 11)
      (gfx/draw-text (str "pushes " hits) 20 92 8)
      (gfx/draw-text (str "snakes " snake-cnt) 20 98 15))
  state)

(defn draw-frame
  [canvas window _frame_id state]
  (when (and (c2d/key-pressed? window)
             (= \0 (c2d/key-char window)))
    (c2d/save canvas (c2d/next-filename "results/games/the_king/" ".jpg")))
  (-> canvas
      (c2d/set-background :black)
      (c2d/set-stroke gfx/point-size :square :miter)
      (c2d/translate 50 25))
  (-> (assoc state :window window :canvas canvas)
      (update-state)
      (draw)))

(def ^:const window-width (+ gfx/canvas-size 100))
(def ^:const window-height (+ gfx/canvas-size 50))

(defn run-game
  ([_] (run-game))
  ([]
   (c2d/show-window {:canvas (c2d/canvas window-width window-height :low)
                     :window-name "The King"
                     :draw-fn draw-frame
                     :fps 30
                     :draw-state (init-state)})))

(comment
  (run-game))
