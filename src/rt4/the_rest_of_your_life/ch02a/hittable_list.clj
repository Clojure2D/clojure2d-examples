(ns rt4.the-rest-of-your-life.ch02a.hittable-list
  (:require [rt4.the-rest-of-your-life.ch02a.interval :as interval]
            [rt4.the-rest-of-your-life.ch02a.hittable :as hittable]
            [rt4.the-rest-of-your-life.ch02a.aabb :as aabb])
  (:import [rt4.the_rest_of_your_life.ch02a.interval Interval]
           [rt4.the_rest_of_your_life.ch02a.hittable HitData]))

(defprotocol HittableListProto
  (add [hittable-list object]))

(defrecord HittableList [objects bbox]
  hittable/HittableProto
  (hit [_ ray ray-t]
    (reduce (fn [curr-hit object]
              (if-let [hit-object (hittable/hit object ray (interval/interval (.mn ^Interval ray-t)
                                                                              (or (and curr-hit
                                                                                       (.t ^HitData curr-hit))
                                                                                  (.mx ^Interval ray-t))))]
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
