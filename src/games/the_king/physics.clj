(ns games.the-king.physics
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [games.the-king.map :as map]))

(defn apply-force
  ([{:keys [angle] :as player} strength]
   (let [angle (* m/TWO_PI angle)]
     (apply-force player (v/vec2 (m/cos angle) (m/sin angle)) strength)))
  ([who direction strength]
   (update who :velocity #(-> direction
                              (v/mult strength)
                              (v/add %)))))

(defn apply-gradient
  [grid strength who]
  (apply-force who (map/gradient grid (v/div (:position who) 8)) strength))

(defn limit-speed
  [who]
  (update who :velocity #(let [l (v/mag %)]
                           (if (< l 0.01)
                             (v/vec2)
                             (v/limit % 2.0)))))

(defn update-actor
  [grid {:keys [velocity position radius] :as who}]
  (let [friction (case (map/elevation grid (v/div position 8))
                   0 0.5
                   1 0.7
                   0.9)
        [nvx nvy :as nvelocity] (v/mult velocity friction)
        [npx npy :as nposition] (v/add position nvelocity)
        [px vx] (cond
                  (< npx radius) [radius (- nvx)]
                  (> npx (- 128 radius)) [(- 128 radius) (- nvx)]
                  :else [npx nvx])
        [py vy] (cond
                  (< npy radius) [radius (- nvy)]
                  (> npy (- 121 radius)) [(- 121 radius) (- nvy)]
                  :else [npy nvy])]
    (assoc who
           :position (v/vec2 px py)
           :velocity (v/vec2 vx vy))))

;; https://stackoverflow.com/questions/73364881/finding-collision-between-two-balls
(defn check-collisions
  [{:keys [king player hits] :as state}]
  (let [{king-position :position
         king-velocity :velocity
         king-mass :mass
         king-radius :radius} king
        {player-position :position
         player-velocity :velocity
         player-mass :mass
         player-radius :radius} player
        dist (v/dist king-position player-position)]
    (if (<= dist (+ king-radius player-radius))
      (let [vec-norm (-> (v/sub king-position player-position)
                         (v/normalize))
            vec-rel-velocity (v/sub player-velocity king-velocity)
            speed (v/sum (v/emult vec-norm vec-rel-velocity))
            nhits (if (> (v/mag player-velocity) 0.25) (inc hits) hits)]
        (if (m/not-neg? speed)
          (let [J (/ (* 2.0 speed) (+ king-mass player-mass))
                npv (v/sub player-velocity (v/mult vec-norm (* J king-mass)))
                nkv (v/add king-velocity (v/mult vec-norm (* J player-mass)))]
            (-> state
                (assoc :hits nhits)
                (assoc-in [:king :velocity] nkv)
                (assoc-in [:player :velocity] npv)))
          (assoc state :hits nhits)))
      state)))
