(ns netgame.core
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream]))

(defmulti write-diff (fn [& args] (first args)))
(defmulti apply-diff (fn [& args] (first args)))

(defmethod write-diff 'int32 [type stream from to]
  (. stream writeInt to))

(defmethod apply-diff 'int32 [type stream old]
  (. stream readInt))

(defmethod write-diff 'int16 [type stream from to]
  (. stream writeShort to))

(defmethod apply-diff 'int16 [type stream old]
  (. stream readShort))

(defmethod write-diff 'int8 [type stream from to]
  (. stream writeByte to))

(defmethod apply-diff 'int8 [type stream old]
  (. stream readByte))

(defn pow2 [] (iterate #(* 2 %) 1))

(defn type-eq [type]
  (case type
    (int8 int16 int32) '==
    (if (class? (resolve type))
      'identical?
      '=)))

(defn gen-write-diff [name field-names field-types]
  `(defmethod write-diff '~name [~'type ~'stream ~'from ~'to]
     (let [~'flags (unchecked-byte
                    (bit-or
                     ~@(map
                        (fn [field type flag]
                          `(if (~(type-eq type) (. ~'from ~field) (. ~'to ~field)) 0 ~flag))
                        field-names
                        field-types
                        (pow2))))]
       (. ~'stream ~'writeByte ~'flags)
       ~@(map
          (fn [field type index]
            `(when (bit-test ~'flags ~index)
               (write-diff '~type ~'stream (. ~'from ~field) (. ~'to ~field))))
          field-names
          field-types
          (range)))))

(defn gen-apply-diff [name field-names field-types]
  `(defmethod apply-diff '~name [~'type ~'stream ~'old]
     (let [~'flags (. ~'stream ~'readByte)]
       (new
        ~name
        ~@(map
           (fn [field type index]
             `(if (bit-test ~'flags ~index)
                (apply-diff '~type ~'stream (. ~'old ~field))
                (. ~'old ~field)))
           field-names
           field-types
           (range))))))

(defmacro def-net-struct [name & fields]
  `(do
     (defrecord ~name ~(apply vector (map first fields)))
     ~(gen-write-diff name (map first fields) (map second fields))
     ~(gen-apply-diff name (map first fields) (map second fields))))

(macroexpand-1 '(def-net-struct Baz
                  (a int32)
                  (b int16)
                  (c int16)
                  (d int32)))

(macroexpand-1 '(def-net-struct Bar
                  (x int16)
                  (y int16)
                  (baz Baz)))

(def-net-struct Baz
  (a int32)
  (b int16)
  (c int16)
  (d int32))

(def-net-struct Bar
  (x int16)
  (y int16)
  (baz Baz))

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      baz (Baz. 1 2 3 4)]
  (write-diff 'Bar dos (Bar. 1 1 baz) (Bar. 2 2 (assoc baz :a 10) ))
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (print (count arr))
    (apply-diff 'Bar dis (Bar. 1 1 baz))))

;(defrecord Foobaz [x])
