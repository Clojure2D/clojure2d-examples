(ns GG.M.M-6-1-03
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(def wname "M-6-1-03")

(def ^:const w 800)
(def ^:const h 800)
(def ^:const radius 100.0)
(def ^:const rradius (/ radius))
(def ^:const strength -5.0)
(def ^:const ramp 1.0)
(def ^:const damping 0.5)
(def ^:const max-velocity 10.0)
(def ^:const node-diameter 16.0)
(def ^:const node-diameter-4 (- node-diameter 4.0))
(def ^:const node-radius (/ node-diameter 2.0))
(def ^:const min-x node-radius)
(def ^:const min-y node-radius)
(def ^:const max-x (- w node-radius))
(def ^:const max-y (- h node-radius))

(def ^:const length 20.0)
(def ^:const stiffness 1.0)
(def ^:const sdamping 0.9)

;; node

(defrecord Node [pos velocity])

(defn update-node
  [^Node node]
  (let [^Vec2 velocity (v/limit (.velocity node) max-velocity)
        ^Vec2 pos (v/add (.pos node) velocity)
        [nx nvx] (cond
                   (< (.x pos) min-x) [(- (* 2.0 min-x) (.x pos))
                                       (- (.x velocity))]
                   (> (.x pos) max-x) [(- (* 2.0 max-x) (.x pos))
                                       (- (.x velocity))]
                   :else [(.x pos) (.x velocity)])
        [ny nvy] (cond
                   (< (.y pos) min-y) [(- (* 2.0 min-y) (.y pos))
                                       (- (.y velocity))]
                   (> (.y pos) max-y) [(- (* 2.0 max-y) (.y pos))
                                       (- (.y velocity))]
                   :else [(.y pos) (.y velocity)])
        nvelocity (v/mult (Vec2. nvx nvy) (- 1.0 damping))]
    (Node. (Vec2. nx ny) nvelocity)))

(defn attract
  [^Node node ^Node other-node]
  (if (identical? node other-node)
    node
    (let [d (v/dist (.pos node) (.pos other-node))]
      (if-not (and (pos? d) (< d radius))
        node
        (let [s (m/pow (* d rradius) (m// ramp))
              f (/ (* s 9.0 strength (+ (m// (inc s))
                                        (m// (- s 3.0) 4.0))) d)
              df (v/mult (v/sub (.pos other-node) (.pos node)) f)]
          (Node. (.pos node) (v/add (.velocity node) df)))))))

(defn attract-nodes
  [nodes]
  (vec (for [node nodes]
         (reduce attract node nodes))))

(defn make-nodes
  ([] (make-nodes 100))
  ([cnt]
   (vec (repeatedly cnt #(Node. (Vec2. (+ (/ w 2) (r/drand -200.0 200.0))
                                       (+ (/ h 2) (r/drand -200.0 200.0)))
                                (Vec2. 0.0 0.0))))))

;; spring

(defrecord Spring [from to])

(defn update-spring
  [nodes ^Spring spring]
  (let [to-id (.to spring)
        from-id (.from spring)
        ^Node to (nodes to-id)
        ^Node from (nodes from-id)
        diff (v/set-mag (v/sub (.pos to) (.pos from)) length)
        target (v/add (.pos from) diff)
        force (-> (v/sub target (.pos to))
                  (v/mult 0.5)
                  (v/mult stiffness)
                  (v/mult (- 1.0 sdamping)))]
    (-> nodes
        (update to-id (fn [^Node node]
                        (Node. (.pos node)
                               (v/add (.velocity node) force))))
        (update from-id (fn [^Node node]
                          (Node. (.pos node)
                                 (v/sub (.velocity node) force)))))))

(defn make-springs
  [cnt]
  (map (fn [id]
         (Spring. id (r/irand (inc id) cnt))) (range (dec cnt))))

;; drawing

(defn find-nearest-node
  [ctx pos]
  (->> (ctx :nodes)
       (map-indexed (fn [id ^Node node]
                      [id (v/dist (.pos node) pos)]))
       (sort-by second)
       (ffirst)
       (assoc ctx :pressed)))

(defn process-mouse
  [ctx state pos]
  (if-not state
    (dissoc ctx :pressed)
    (if-let [id (ctx :pressed)]
      (update-in ctx [:nodes id] (fn [^Node node]
                                   (Node. pos (.velocity node))))
      (find-nearest-node ctx pos))))

(defn draw [canvas window _frame ctx]
  (c2d/set-background canvas :white)
  (c2d/set-color canvas [0 130 164])
  (c2d/set-stroke canvas 2)
  (let [{:keys [springs nodes] :as ctx} (process-mouse ctx (c2d/get-state window) (c2d/mouse-pos window))
        new-nodes (attract-nodes nodes)
        new-nodes (reduce update-spring new-nodes springs)
        new-nodes (mapv update-node new-nodes)]
    (doseq [{:keys [from to]} springs
            :let [start (.pos ^Node (new-nodes from))
                  end (.pos ^Node (new-nodes to))]]
      (c2d/line canvas start end))
    (doseq [^Node node new-nodes]
      (-> canvas
          (c2d/set-color :white)
          (c2d/ellipse (.pos node) node-diameter node-diameter)
          (c2d/set-color :black)
          (c2d/ellipse (.pos node) node-diameter-4 node-diameter-4)))
    (assoc ctx :nodes new-nodes)))

(def window (let [state (let [nodes (make-nodes 200)]
                        {:springs (distinct (make-springs (count nodes)))
                         :nodes nodes})]
            (c2d/show-window {:canvas (c2d/canvas w h)
                              :draw-fn draw
                              :window-name wname
                              :draw-state state})))

(defmethod c2d/mouse-event [wname :mouse-pressed] [_ _] true)
(defmethod c2d/mouse-event [wname :mouse-released] [_ _] nil)
