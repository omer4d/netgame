(ns netgame.core
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream])
  (:use clojure.data))

(defmulti write-bin (fn [& args] (first args)))
(defmulti read-bin (fn [& args] (first args)))
(defmulti write-diff (fn [& args] (first args)))
(defmulti apply-diff (fn [& args] (first args)))

;; Int

(defmethod write-bin 'int [type ^DataOutputStream stream x]
  (. stream writeInt x))

(defmethod read-bin 'int [type ^DataInputStream stream]
  (. stream readInt))

(defmethod write-diff 'int [type ^DataOutputStream stream from to]
  (. stream writeInt to))

(defmethod apply-diff 'int [type ^DataInputStream stream old]
  (. stream readInt))

;; Short

(defmethod write-bin 'short [type ^DataOutputStream stream x]
  (. stream writeShort x))

(defmethod read-bin 'short [type ^DataInputStream stream]
  (. stream readShort))

(defmethod write-diff 'short [type ^DataOutputStream stream from to]
  (. stream writeShort to))

(defmethod apply-diff 'short [type ^DataInputStream stream old]
  (. stream readShort))

;; Byte

(defmethod write-bin 'byte [type ^DataOutputStream stream x]
  (. stream writeByte x))

(defmethod read-bin 'byte [type ^DataInputStream stream]
  (. stream readByte))

(defmethod write-diff 'byte [type ^DataOutputStream stream from to]
  (. stream writeByte to))

(defmethod apply-diff 'byte [type ^DataInputStream stream old]
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

(defn gen-write-bin [name field-names field-types]
  (let [stream-sym (tagged-sym 'stream `DataOutputStream)
        x-sym (tagged-sym 'x name)]
    `(defmethod write-bin '~name [~'type ~stream-sym ~x-sym]
       ~@(map
          (fn [field type]
            `(write-bin '~type ~stream-sym (. ~x-sym ~field)))
          field-names
          field-types))))

(defn gen-read-bin [name client-rec-name field-names field-types]
  (let [stream-sym (tagged-sym 'stream `DataInputStream)]
    `(defmethod read-bin '~name [~'type ~stream-sym]
       (new
        ~client-rec-name
        ~@(map
           (fn [field type]
             `(read-bin '~type ~stream-sym))
           field-names
           field-types)))))

(defn gen-write-diff [name field-names field-types]
  (let [stream-sym (tagged-sym 'stream `DataOutputStream)
        from-sym (tagged-sym 'from name)
        to-sym (tagged-sym 'to name)]
    `(defmethod write-diff '~name [~'type ~stream-sym ~from-sym ~to-sym]
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
            (fn [field type index]
              `(when (bit-test ~'flags ~index)
                 (write-diff '~type ~stream-sym (. ~from-sym ~field) (. ~to-sym ~field))))
            field-names
            field-types
            (range))))))

(defn gen-apply-diff [name client-rec-name field-names field-types]
  (let [stream-sym (tagged-sym 'stream `DataInputStream)
        old-sym (tagged-sym 'old client-rec-name)]
    `(defmethod apply-diff '~name [~'type ~stream-sym ~old-sym]
       (let [~'flags (. ~stream-sym ~'readByte)]
         (new
          ~client-rec-name
          ~@(map
             (fn [field type index]
               `(if (bit-test ~'flags ~index)
                  (apply-diff '~type ~stream-sym (. ~old-sym ~field))
                  (. ~old-sym ~field)))
             field-names
             field-types
             (range)))))))

(defn plist->map [plist]
  (into {} (map #(into [] %)(partition 2 plist))))

(defn canonize-list-field [field]
  {:name (first field)
   :type (second field)
   :options (plist->map (drop 2 field))})

(defn canonize-nf-field [field]
  (canonize-list-field field))

(defn canonize-sf-field [field]
  (if (list? field)
    (canonize-list-field field)
    {:name field
     :type 'Object
     :options {}}))

(defmacro def-net-struct [struct-name & field-group-plist]
  (let [field-groups (plist->map field-group-plist)
        canon-nf-fields (map canonize-nf-field (:net field-groups))
        canon-sf-fields (map canonize-sf-field (:server field-groups))
        client-rec-name (symbol (str (name struct-name) "Client"))]
    `(do
       (defrecord ~struct-name ~(apply vector (map #(tagged-sym (:name %) (:type %)) (concat canon-nf-fields canon-sf-fields))))
       (defrecord ~client-rec-name ~(apply vector (map #(tagged-sym (:name %) (:type %)) canon-nf-fields)))
       ~(gen-write-diff struct-name (map :name canon-nf-fields) (map :type canon-nf-fields))
       ~(gen-apply-diff struct-name client-rec-name (map :name canon-nf-fields) (map :type canon-nf-fields))
       ~(gen-write-bin struct-name (map :name canon-nf-fields) (map :type canon-nf-fields))
       ~(gen-read-bin struct-name client-rec-name (map :name canon-nf-fields) (map :type canon-nf-fields)))))

;;
;; Tests
;;

'(def-net-struct Foo
   :net [(a int)
         (b short)
         (c short)
         (d int :interp linear)]
   :server [(x)
            (y)
            (z int)])

(macroexpand-1 '(def-net-struct Baz
                  :net [(a int)
                        (b short)
                        (c short)
                        (d int)]
                  :server [x y z]))

(macroexpand-1 '(def-net-struct Bar
                  :net [(x short)
                        (y short)
                        (baz Baz)]))

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
   (baz Baz)])

;; TODO:
;; full updates - done
;; interpolation
;; int map diffs
;; def-net-msg
;; syntax validation + option validation + ensure net struct has at least 1 net property
;; flag 0 -> field changed to nil
;; add options param

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      baz (Baz. 1 2 3 4 'a 'b 'c)]
  (write-diff 'Bar dos (Bar. 1 1 baz) (Bar. 2 2 (assoc baz :a 10) ))
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (print (count arr))
    (apply-diff 'Bar dis (BarClient. 1 1 (BazClient. 1 2 3 4)))))

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      baz (Baz. 1 2 3 4 'a 'b 'c)]
  (write-bin 'Bar dos (Bar. 1 1 baz))
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (read-bin 'Bar dis)))
