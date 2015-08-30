(ns netgame.game
  (:require [netgame.core :refer [def-net-struct write-bin read-bin write-diff apply-diff]])
  (:require [quil.core :as q])
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream]))

(defmacro with-fields [rec fields & body]
  (let [tmp-sym (gensym)]
    `(let [~tmp-sym ~rec
           ~@(mapcat (fn [field] `(~field (. ~tmp-sym ~field))) fields)]
       ~@body)))

(def-net-struct Entity
  :net
  [(x float)
   (y float)
   (rad float)
   (red byte)
   (green byte)
   (blue byte)]
  :server
  [(vx float)
   (vy float)])

(def-net-struct Game
  :net
  [(ents (short-map :elem-type Entity))])

(defn randf ^double [^double min ^double max]
  (+ min (* (double (rand)) (- max min))))

(defn rand-sig ^double []
  (if (< (double (rand)) 0.5) 1.0 -1.0))

(defn create-entity [x y]
  (Entity. x y
           (randf 10 30)
           (byte (rand-int 256))
           (byte (rand-int 255))
           (byte (rand-int 255))
           (* (rand-sig) (randf 50 100))
           (* (rand-sig) (randf 50 100))))

(defn create-game [n]
  (Game. (zipmap (range)
                 (take n (repeatedly #(create-entity (randf 0 640) (randf 0 480)))))))



(defn update-entity [^Entity ent ^double dt]
  ent)

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 200))

(defn draw []
  (q/stroke (q/random 255))
  (q/stroke-weight (q/random 10))
  (q/fill (q/random 255))

  (let [diam (q/random 100)
        x    (q/random (q/width))
        y    (q/random (q/height))]
    (q/ellipse x y diam diam)))

(q/defsketch example
  :title "Oh so many grey circles"
  :setup setup
  :draw draw
  :size [323 200])
