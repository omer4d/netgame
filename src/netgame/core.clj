(ns netgame.core
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream])
  (:use clojure.data))

(defmulti write-bin (fn [type stream x type-opts] type))
(defmulti read-bin (fn [type stream type-opts] type))
(defmulti write-diff (fn [type stream from to type-opts] type))
(defmulti apply-diff (fn [type stream fold type-opts] type))

;; Int

(defmethod write-bin 'int [type ^DataOutputStream stream x _]
  (. stream writeInt x))

(defmethod read-bin 'int [type ^DataInputStream stream _]
  (. stream readInt))

(defmethod write-diff 'int [type ^DataOutputStream stream from to _]
  (. stream writeInt to))

(defmethod apply-diff 'int [type ^DataInputStream stream old _]
  (. stream readInt))

;; Short

(defmethod write-bin 'short [type ^DataOutputStream stream x _]
  (. stream writeShort x))

(defmethod read-bin 'short [type ^DataInputStream stream _]
  (. stream readShort))

(defmethod write-diff 'short [type ^DataOutputStream stream from to _]
  (. stream writeShort to))

(defmethod apply-diff 'short [type ^DataInputStream stream old _]
  (. stream readShort))

;; Byte

(defmethod write-bin 'byte [type ^DataOutputStream stream x _]
  (. stream writeByte x))

(defmethod read-bin 'byte [type ^DataInputStream stream _]
  (. stream readByte))

(defmethod write-diff 'byte [type ^DataOutputStream stream from to _]
  (. stream writeByte to))

(defmethod apply-diff 'byte [type ^DataInputStream stream old _]
  (. stream readByte))

(defn pow2 [] (iterate #(* 2 %) 1))

(defn type-eq [type]
  (case type
    (byte short int) '==
    (if (class? (resolve type))
      'identical?
      '=)))

(defn tagged-sym [sym tag]
  (with-meta sym {:tag tag}))

(defn gen-write-bin [name field-names field-types field-type-opts]
  (let [stream-sym (tagged-sym 'stream `DataOutputStream)
        x-sym (tagged-sym 'x name)]
    `(defmethod write-bin '~name [~'type ~stream-sym ~x-sym ~'_]
       ~@(map
          (fn [field type type-opts]
            `(write-bin '~type ~stream-sym (. ~x-sym ~field) ~type-opts))
          field-names
          field-types
          field-type-opts))))

(defn gen-read-bin [name client-rec-name field-names field-types field-type-opts]
  (let [stream-sym (tagged-sym 'stream `DataInputStream)]
    `(defmethod read-bin '~name [~'type ~stream-sym ~'_]
       (new
        ~client-rec-name
        ~@(map
           (fn [field type type-opts]
             `(read-bin '~type ~stream-sym ~type-opts))
           field-names
           field-types
           field-type-opts)))))

(defn gen-write-diff [name field-names field-types field-type-opts]
  (let [stream-sym (tagged-sym 'stream `DataOutputStream)
        from-sym (tagged-sym 'from name)
        to-sym (tagged-sym 'to name)]
    `(defmethod write-diff '~name [~'type ~stream-sym ~from-sym ~to-sym ~'_]
       (let [~'flags (unchecked-byte
                      (bit-or
                       ~@(map
                          (fn [field type flag]
                            `(if (~(type-eq type) (. ~from-sym ~field) (. ~to-sym ~field)) 0 ~flag))
                          field-names
                          field-types
                          (pow2))))]
         (. ~stream-sym ~'writeByte ~'flags)
         ~@(map
            (fn [field type type-opts index]
              `(when (bit-test ~'flags ~index)
                 (write-diff '~type ~stream-sym (. ~from-sym ~field) (. ~to-sym ~field) ~type-opts)))
            field-names
            field-types
            field-type-opts
            (range))))))

(defn gen-apply-diff [name client-rec-name field-names field-types field-type-opts]
  (let [stream-sym (tagged-sym 'stream `DataInputStream)
        old-sym (tagged-sym 'old client-rec-name)]
    `(defmethod apply-diff '~name [~'type ~stream-sym ~old-sym ~'_]
       (let [~'flags (. ~stream-sym ~'readByte)]
         (new
          ~client-rec-name
          ~@(map
             (fn [field type type-opts index]
               `(if (bit-test ~'flags ~index)
                  (apply-diff '~type ~stream-sym (. ~old-sym ~field) ~type-opts)
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
       (defrecord ~struct-name ~(apply vector (map #(tagged-sym (:name %) (:type %)) (concat canon-nf canon-sf))))
       (defrecord ~client-rec-name ~(apply vector (map #(tagged-sym (:name %) (:type %)) canon-nf)))
       ~(gen-write-diff struct-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-apply-diff struct-name client-rec-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-write-bin struct-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf))
       ~(gen-read-bin struct-name client-rec-name (map :name canon-nf) (map :type canon-nf) (map :type-opts canon-nf)))))

;;
;; Tests
;;

'(def-net-struct Foo
   :net [(a int)
         (b short)
         (c short)
         (d int :interp linear)
         (e (short-map :val-type Baz) :interp linear)]
   :server [(x)
            (y)
            (z int)])

(macroexpand-1 '(def-net-struct Baz
                  :net [(a int)
                        (b short)
                        (c short)
                        (d int)]
                  :server [(x int :p 1 :q 2) y z]))

(macroexpand-1 '(def-net-struct Bar
                  :net [(x short)
                        (y short)
                        (baz (Baz :a 1 :b 2))]))

(def-net-struct Baz
  :net
  [(a int)
   (b short)
   (c short)
   (d int)]
  :server
  [x y z])

(def-net-struct Bar
  :net
  [(x short)
   (y short)
   (baz (Baz :a 1 :b 2))])

;; TODO:
;; full updates - done
;; add type options param - done

;; interpolation
;; int map diffs
;; def-net-msg
;; syntax validation + option validation + ensure net struct has at least 1 net property
;; flag 0 -> field changed to nil
;; write some unit tests

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      baz (Baz. 1 2 3 4 'a 'b 'c)]
  (write-diff 'Bar dos (Bar. 1 1 baz) (Bar. 2 2 (assoc baz :a 10)) nil)
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (print (count arr))
    (apply-diff 'Bar dis (BarClient. 1 1 (BazClient. 1 2 3 4)) nil)))

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      baz (Baz. 1 2 3 4 'a 'b 'c)]
  (write-bin 'Bar dos (Bar. 1 1 baz) nil)
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (read-bin 'Bar dis nil)))
