;; *****************
;; * game_misc.clj *
;; *****************

(in-ns 'game)

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
    (if (> x max) max x)))

(defn nano->sec ^double [^long x] (/ (double x) 1000000000.0))
