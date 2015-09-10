;; *******************
;; * game_engine.clj *
;; *******************

(in-ns 'netgame.game)

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
  [(ents (short-map :elem-type 'Entity))])

(defn create-entity [x y]
  (Entity. x y
           (randf 10 30)
           (unchecked-byte (rand-int 256))
           (unchecked-byte (rand-int 255))
           (unchecked-byte (rand-int 255))
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
