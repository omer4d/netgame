(ns netgame.core
  (:import [java.nio ByteBuffer])
  (:use clojure.data))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmulti write-bin (fn [type buff x type-opts] type))
(defmulti read-bin (fn [type buff type-opts] type))
(defmulti write-diff (fn [type buff from to type-opts] type))
(defmulti apply-diff (fn [type buff old type-opts] type))

(defn tagged-sym [sym tag]
  (with-meta sym {:tag tag}))

(defn safe-tagged-sym [sym tag]
  (if (or (contains? '#{int short byte float string} tag) (class? (resolve tag)))
    (with-meta sym {:tag tag})
    sym))

(defmacro gen-primitive-methods [type write-method read-method]
  (let [buff-sym (tagged-sym 'buff 'ByteBuffer)]
    `(do
       (defmethod write-bin '~type [~'type ~buff-sym ~'x ~'_]
         (. ~buff-sym ~write-method ~'x))
       (defmethod read-bin '~type [~'type ~buff-sym ~'_]
         (. ~buff-sym ~read-method))
       (defmethod write-diff '~type [~'type ~buff-sym ~'from ~'to ~'_]
         (. ~buff-sym ~write-method ~'to))
       (defmethod apply-diff '~type [~'type ~buff-sym ~'old ~'_]
         (. ~buff-sym ~read-method)))))

(gen-primitive-methods int putInt getInt)
(gen-primitive-methods short putShort getShort)
(gen-primitive-methods float putFloat getFloat)

(defmethod write-bin 'byte [type ^ByteBuffer buff x _] (. buff put (byte x)))
(defmethod read-bin 'byte [type ^ByteBuffer buff _] (. buff get))
(defmethod write-diff 'byte [type ^ByteBuffer buff from to _] (. buff put (byte to)))
(defmethod apply-diff 'byte [type ^ByteBuffer buff old _] (. buff get))

;; short-map

(defmethod write-bin 'short-map [type ^ByteBuffer buff x type-opts]
  (. buff putShort (count x))
  (let [elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-write-bin (get-method write-bin elem-type)]
    (doseq [[k v] x]
      (. buff putShort k)
      (val-write-bin elem-type buff v elem-type-opts))))

(defmethod read-bin 'short-map [type ^ByteBuffer buff type-opts]
  (let [out (transient {})
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-read-bin (get-method read-bin elem-type)]
    (dotimes [_ (. buff getShort)]
      (assoc! out (. buff getShort) (val-read-bin elem-type buff elem-type-opts)))
    (persistent! out)))

(defmethod write-diff 'short-map [type ^ByteBuffer buff from to type-opts]
  (let [[removed added shared] (diff (set (keys from)) (set (keys to)))
        changed (filter #((complement identical?) (get from %) (get to %)) shared)
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-write-diff (get-method write-diff elem-type)
        val-write-bin (get-method write-bin elem-type)]
    (. buff putShort (count removed))
    (doseq [key removed]
      (. buff putShort key))
    (. buff putShort (count added))
    (doseq [key added]
      (. buff putShort key)
      (val-write-bin elem-type buff (get to key) elem-type-opts))
    (. buff putShort (count changed))
     (doseq [key changed]
      (. buff putShort key)
      (val-write-diff elem-type buff (get from key) (get to key) elem-type-opts))))

(defmethod apply-diff 'short-map [type ^ByteBuffer buff old type-opts]
  (let [tmp (transient old)
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-apply-diff (get-method apply-diff elem-type)
        val-read-bin (get-method read-bin elem-type)]
    (dotimes [_ (. buff getShort)]
      (dissoc! tmp (. buff getShort)))
    (dotimes [_ (. buff getShort)]
      (assoc! tmp (. buff getShort) (val-read-bin elem-type buff elem-type-opts)))
    (dotimes [_ (. buff getShort)]
      (let [key (. buff getShort)]
        (assoc! tmp key (val-apply-diff elem-type buff (get old key) elem-type-opts))))
    (persistent! tmp)))

;; Etc.

(defn pow2 [] (iterate #(* 2 %) 1))

(defn type-eq [type]
  (case type
    (byte short int) '==
    (if (class? (resolve type))
      'identical?
      '=)))

(defn get-write-method [^long bits]
  (cond (<= bits 8) 'put
        (<= bits 16) 'putShort
        (<= bits 32) 'putInt
        true 'putLong))

(defn get-read-method [^long bits]
  (cond (<= bits 8) 'get
        (<= bits 16) 'getShort
        (<= bits 32) 'getInt
        true 'getLong))

(defn gen-write-bin [name field-names field-types field-type-opts]
  (let [buff-sym (tagged-sym 'buff `ByteBuffer)
        x-sym (tagged-sym 'x name)]
    `(defmethod write-bin '~name [~'type ~buff-sym ~x-sym ~'_]
       ~@(map
          (fn [field type type-opts]
            `(write-bin '~type ~buff-sym (. ~x-sym ~field) ~type-opts))
          field-names
          field-types
          field-type-opts))))

(defn gen-read-bin [name client-rec-name field-names field-types field-type-opts]
  (let [buff-sym (tagged-sym 'buff `ByteBuffer)]
    `(defmethod read-bin '~name [~'type ~buff-sym ~'_]
       (new
        ~client-rec-name
        ~@(map
           (fn [field type type-opts]
             `(read-bin '~type ~buff-sym ~type-opts))
           field-names
           field-types
           field-type-opts)))))

(defn gen-write-diff [name field-names field-types field-type-opts]
  (let [buff-sym (tagged-sym 'buff `ByteBuffer)
        from-sym (tagged-sym 'from name)
        to-sym (tagged-sym 'to name)]
    `(defmethod write-diff '~name [~'type ~buff-sym ~from-sym ~to-sym ~'_]
       (let [~'flags (unchecked-byte
                      (bit-or 0
                       ~@(map
                          (fn [field type flag]
                            `(if (~(type-eq type) (. ~from-sym ~field) (. ~to-sym ~field)) 0 ~flag))
                          field-names
                          field-types
                          (pow2))))]
         (. ~buff-sym ~(get-write-method (count field-names)) ~'flags)
         ~@(map
            (fn [field type type-opts index]
              `(when (bit-test ~'flags ~index)
                 (write-diff '~type ~buff-sym (. ~from-sym ~field) (. ~to-sym ~field) ~type-opts)))
            field-names
            field-types
            field-type-opts
            (range))))))

(defn gen-apply-diff [name client-rec-name field-names field-types field-type-opts]
  (let [buff-sym (tagged-sym 'buff `ByteBuffer)
        old-sym (tagged-sym 'old client-rec-name)]
    `(defmethod apply-diff '~name [~'type ~buff-sym ~old-sym ~'_]
       (let [~'flags (. ~buff-sym ~(get-read-method (count field-names)))]
         (new
          ~client-rec-name
          ~@(map
             (fn [field type type-opts index]
               `(if (bit-test ~'flags ~index)
                  (apply-diff '~type ~buff-sym (. ~old-sym ~field) ~type-opts)
                  (. ~old-sym ~field)))
             field-names
             field-types
             field-type-opts
             (range)))))))

(defn plist->map [plist]
  (into {} (map #(into [] %)(partition 2 plist))))

(defn canonize-list-field [field]
  (let [type+opts (second field)]
    {:name (first field)
     :type (if (symbol? type+opts) type+opts (first type+opts))
     :type-opts (if (symbol? type+opts) nil (plist->map (rest type+opts)))
     :field-opts (plist->map (drop 2 field))}))

(defn canonize-nf [field]
  (canonize-list-field field))

(defn canonize-sf [field]
  (if (list? field)
    (canonize-list-field field)
    {:name field
     :type 'Object
     :type-opts {}
     :field-opts {}}))

(defmacro def-net-struct [struct-name & field-group-plist]
  (let [field-groups (plist->map field-group-plist)
        canon-nf (map canonize-nf (:net field-groups))
        canon-sf (map canonize-sf (:server field-groups))
        client-rec-name (symbol (str (name struct-name) "Client"))]
    `(do
       (defrecord ~struct-name ~(apply vector (map #(safe-tagged-sym (:name %) (:type %)) (concat canon-nf canon-sf))))
       (defrecord ~client-rec-name ~(apply vector (map #(safe-tagged-sym (:name %) (:type %)) canon-nf)))
       ~(gen-write-diff struct-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-apply-diff struct-name client-rec-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-write-bin struct-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-read-bin struct-name client-rec-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf)))))

(defmacro defbin [rec-name & fields]
  (let [canon-fields (map canonize-sf fields)]
    `(do
       (defrecord ~rec-name ~(apply vector (map #(tagged-sym (:name %) (:type %)) canon-fields)))
       ~(gen-write-bin rec-name (map :name canon-fields) (map :type canon-fields) (map :type-opts canon-fields))
       ~(gen-read-bin rec-name rec-name (map :name canon-fields) (map :type canon-fields) (map :type-opts canon-fields)))))

;; (macroexpand-1 '(defbin MsgHeader
;;                   (type byte)
;;                   (ver byte)))

;; (macroexpand-1 '(def-net-struct Baz
;;                   :net [(a int)
;;                         (b short)
;;                         (c short)
;;                         (d int)]
;;                   :server [(x int :p 1 :q 2) y z]))

;; (macroexpand-1 '(def-net-struct Bar
;;                   :net [(x short)
;;                         (y short)
;;                         (baz (Baz :a 1 :b 2))]))

;; TODO:
;; full updates - done
;; add type options param - done
;; int map diffs - done
;; handle case where record identities are different but atom leaves are identical - done, write 0 and let compression handle it
;; write some unit tests - done
;; handle records with > 8 fields - done
;; handle floats - done
;; handle strings - done
;; def-net-msg - done, use two defbins

;; handle record field change to nil (Maybe? types)
;; handle short map element change to nil (Maybe? types)
;; interpolation
;; syntax validation + option validation + ensure net struct has at least 1 net property
;; figure out replacement for safe-tagged-sym
