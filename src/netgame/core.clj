(ns netgame.core
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream]))

(defmulti write-diff (fn [& args] (first args)))
(defmulti apply-diff (fn [& args] (first args)))

(defmethod write-diff 'int [type ^DataOutputStream stream from to]
  (. stream writeInt to))

(defmethod apply-diff 'int [type ^DataInputStream stream old]
  (. stream readInt))

(defmethod write-diff 'short [type ^DataOutputStream stream from to]
  (. stream writeShort to))

(defmethod apply-diff 'short [type ^DataInputStream stream old]
  (. stream readShort))

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
  
(defn gen-apply-diff [name field-names field-types]
  (let [stream-sym (tagged-sym 'stream `DataInputStream)
        old-sym (tagged-sym 'old name)]
    `(defmethod apply-diff '~name [~'type ~stream-sym ~old-sym]
       (let [~'flags (. ~stream-sym ~'readByte)]
         (new
          ~name
          ~@(map
             (fn [field type index]
               `(if (bit-test ~'flags ~index)
                  (apply-diff '~type ~stream-sym (. ~old-sym ~field))
                  (. ~old-sym ~field)))
             field-names
             field-types
             (range)))))))

(defmacro def-net-struct [name & fields]
  `(do
     (defrecord ~name ~(apply vector (map #(tagged-sym (first %) (second %)) fields)))
     ~(gen-write-diff name (map first fields) (map second fields))
     ~(gen-apply-diff name (map first fields) (map second fields))))

(macroexpand-1 '(def-net-struct Baz
                  (a int)
                  (b short)
                  (c short)
                  (d int)))

(macroexpand-1 '(def-net-struct Bar
                  (x short)
                  (y short)
                  (baz Baz)))

(def-net-struct Baz
  (a int)
  (b short)
  (c short)
  (d int))

(def-net-struct Bar
  (x short)
  (y short)
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
