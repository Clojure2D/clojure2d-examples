(ns rt4.the-next-week.ch09.hittable-list
  (:require [rt4.the-next-week.ch09.interval :as interval]
            [rt4.the-next-week.ch09.hittable :as hittable]
            [rt4.the-next-week.ch09.aabb :as aabb]))

(defprotocol HittableListProto
  (add [hittable-list object]))

(defrecord HittableList [objects bbox]
  hittable/HittableProto
  (hit [_ ray ray-t]
    (reduce (fn [curr-hit object]
              (if-let [hit-object (hittable/hit object ray (interval/interval (:mn ray-t)
                                                                              (or (:t curr-hit) (:mx ray-t))))]
                hit-object
                curr-hit)) nil objects))
  HittableListProto
  (add [_ object]
    (->HittableList (conj objects object) (aabb/merge-boxes bbox (:bbox object)))))

(defn hittable-list
  [& objects]
  (->HittableList objects (reduce aabb/merge-boxes (map :bbox objects))))

(defn add-all
  [lst objects]
  (reduce add lst objects))

(defn merge-hittable-lists
  [l1 l2]
  (add-all l1 (:objects l2)))
