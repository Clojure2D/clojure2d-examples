(ns rt4.the-next-week.ch08b.hittable
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [rt4.the-next-week.ch08b.aabb :as aabb]
            [rt4.the-next-week.ch08b.ray :as ray])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol HittableProto
  (hit [object r ray-t]))

(defrecord HitData [p normal mat ^double t u v front-face?])

(defn hit-data
  ([r {:keys [p normal mat t u v]
       :or {u 0.0 v 0.0}}]
   (hit-data r p normal mat t u v))
  ([r p normal mat t]
   (hit-data r p normal mat t 0.0 0.0))
  ([r p normal mat t u v]
   (let [front-face? (neg? (v/dot (:direction r) normal))]
     (->HitData p (if front-face? normal (v/sub normal)) mat t u v front-face?))))

;;

(defrecord Translate [object offset bbox]
  HittableProto
  (hit [_ r ray-t]
    (let [offset-r (ray/ray (v/sub (:origin r) offset) (:direction r) (:time r))]
      (when-let [rec (hit object offset-r ray-t)]
        (update rec :p v/add offset)))))

(defn translate [p displacement]
  (->Translate p displacement (aabb/shift (:bbox p) displacement)))

;;

(defmacro ^:private rot+
  [x1 x2]
  `(+ (* ~'sin-theta ~x1) (* ~'cos-theta ~x2)))

(defmacro ^:private rot-
  [x1 x2]
  `(- (* ~'cos-theta ~x1) (* ~'sin-theta ~x2)))


(defrecord RotateY [object ^double sin-theta ^double cos-theta bbox]
  HittableProto
  (hit [_ r ray-t]
    (let [^Vec3 origin (:origin r)
          ^Vec3 direction (:direction r)
          origin (v/vec3 (rot- (.x origin) (.z origin))
                         (.y origin)
                         (rot+ (.x origin) (.z origin)))
          direction (v/vec3 (rot- (.x direction) (.z direction))
                            (.y direction)
                            (rot+ (.x direction) (.z direction)))
          rotated-r (ray/ray origin direction (:time r))]
      (when-let [rec (hit object rotated-r ray-t)]
        (let [^Vec3 p (:p rec)
              p (v/vec3 (rot+ (.z p) (.x p)) (.y p) (rot- (.z p) (.x p)))
              ^Vec3 normal (:normal rec)
              normal (v/vec3 (rot+ (.z normal) (.x normal))
                             (.y normal)
                             (rot- (.z normal) (.x normal)))]
          (assoc rec :p p :normal normal))))))

(defn rotate-y [p ^double angle]
  (let [radians (m/radians angle)
        sin-theta (m/sin radians)
        cos-theta (m/cos radians)
        bbox (:bbox p)
        new-corners (for [^double x [(:mn (:x bbox)) (:mx (:x bbox))]
                          ^double y [(:mn (:y bbox)) (:mx (:y bbox))]
                          ^double z [(:mn (:z bbox)) (:mx (:z bbox))]
                          :let [newx (rot+ z x)
                                newz (rot- z x)]]
                      (v/vec3 newx y newz))
        [mn mx] (reduce (fn [[mn mx] tester]
                          [(v/emn mn tester)
                           (v/emx mx tester)]) [(v/vec3 ##Inf ##Inf ##Inf)
                                                (v/vec3 ##-Inf ##-Inf ##-Inf)] new-corners)]
    (->RotateY p sin-theta cos-theta (aabb/aabb mn mx))))
