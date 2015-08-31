(ns netgame.game
  (:require [netgame.core :refer [def-net-struct write-bin read-bin write-diff apply-diff]])
  (:require [quil.core :as q])
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream]))

(defmacro with-fields [rec fields & body]
  (let [tmp-sym (gensym)]
    `(let [~tmp-sym ~rec
           ~@(mapcat (fn [field] `(~field (. ~tmp-sym ~field))) fields)]
       ~@body)))

(defmacro updated-instance [cls-sym old fields & vals]
  (let [^Class cls (resolve cls-sym)
        basis (clojure.lang.Reflector/invokeStaticMethod cls "getBasis" (to-array []))
        tmp-sym (gensym)
        fvs (zipmap fields vals)]
    `(let [~tmp-sym ~old]
       (new ~cls
            ~@(map #(if (contains? fvs %)
                      (get fvs %)
                      `(. ~tmp-sym ~%))
                   basis)))))

(defn fmap [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn randf ^double [^double min ^double max]
  (+ min (* (double (rand)) (- max min))))

(defn rand-sig ^double []
  (if (< (double (rand)) 0.5) 1.0 -1.0))

(defn in-range? [x min max]
  (and (>= x min) (<= x max)))

(defn clamp [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))

(defn nano->sec ^double [^long x] (/ (double x) 1000000000.0))

;; *************************
;; * Game State Structures *
;; *************************

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

(defn move [{:keys [x y vx vy] :as ent} dt]
  (assoc ent
         :x (+ x (* vx dt))
         :y (+ y (* vy dt))))

(defn accel [{:keys [vx vy] :as ent} ax ay dt]
  (assoc ent
         :vx (+ vx (* ax dt))
         :vy (+ vy (* ay dt))))

(defn bounce [{:keys [x y vx vy] :as ent} minx miny maxx maxy dt]
  (assoc ent
         :x (clamp x minx maxx)
         :y (clamp y miny maxy)
         :vx (if (in-range? x minx maxx) vx (* vx -1))
         :vy (if (in-range? y miny maxy) vy (* vy -1))))

(defn update-entity [{rad :rad :as ent} dt]
  (-> ent (accel 0 100 dt) (move dt) (bounce rad rad (- 640 rad) (- 480 rad) dt)))

(defn update-game [game dt]
  (assoc game :ents (fmap #(update-entity % dt) (:ents game))))

(defn draw-ent [{:keys [x y rad] :as ent}]
  (q/ellipse x y (* rad 2) (* rad 2)))

(defn draw-game [game]
  (doseq [[_ ent] (:ents game)]
    (draw-ent ent)))

;; *************
;; * Processes *
;; *************

(defrecord ServerState [clients game])

(def game-state (atom (create-game 10)))

(def server (atom nil))

(defn stop-server! []
  (future-cancel @server))

(defn start-server! [target-dt]
  (when-not (or (nil? @server) (future-done? @server))
    (stop-server!))
  (reset! server
          (future (loop [last-tick-time 0]
                    (let [curr-time (System/nanoTime)]
                      (when-not (Thread/interrupted)
                        (if (>= (nano->sec (- curr-time last-tick-time)) target-dt)
                          (do
                            (swap! game-state update-game target-dt)
                            (recur curr-time))
                          (recur last-tick-time))))))))

(defn setup []
  (q/smooth)
  (q/frame-rate 60))

(defn draw []
  (q/background 200)
  (draw-game @game-state))

(defn start-client! []
  (q/defsketch example
    :title "Oh so many grey circles"
    :setup setup
    :draw draw
    :size [640 480]))

;(start-server! 0.025)
