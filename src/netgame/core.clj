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

;; short-map

(defmethod write-bin 'short-map [type ^DataOutputStream stream x type-opts]
  (. stream writeShort (count x))
  (let [elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-write-bin (get-method write-bin elem-type)]
    (doseq [[k v] x]
      (. stream writeShort k)
      (val-write-bin elem-type stream v elem-type-opts))))

(defmethod read-bin 'short-map [type ^DataInputStream stream type-opts]
  (let [out (transient {})
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-read-bin (get-method read-bin elem-type)]
    (dotimes [_ (. stream readShort)]
      (assoc! out (. stream readShort) (val-read-bin elem-type stream elem-type-opts)))
    (persistent! out)))

(defmethod write-diff 'short-map [type ^DataOutputStream stream from to type-opts]
  (let [[removed added shared] (diff (set (keys from)) (set (keys to)))
        changed (filter #((complement identical?) (get from %) (get to %)) shared)
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-write-diff (get-method write-diff elem-type)
        val-write-bin (get-method write-bin elem-type)]
    (. stream writeShort (count removed))
    (doseq [key removed]
      (. stream writeShort key))
    (. stream writeShort (count added))
    (doseq [key added]
      (. stream writeShort key)
      (val-write-bin elem-type stream (get to key) elem-type-opts))
    (. stream writeShort (count changed))
    (doseq [key changed]
      (. stream writeShort key)
      (val-write-diff elem-type stream (get from key) (get to key) elem-type-opts))))

(defmethod apply-diff 'short-map [type ^DataInputStream stream old type-opts]
  (let [tmp (transient old)
        elem-type (:elem-type type-opts)
        elem-type-opts (:elem-type-opts type-opts)
        val-apply-diff (get-method apply-diff elem-type)
        val-read-bin (get-method read-bin elem-type)]
    (dotimes [_ (. stream readShort)]
      (dissoc! tmp (. stream readShort)))
    (dotimes [_ (. stream readShort)]
      (assoc! tmp (. stream readShort) (val-read-bin elem-type stream elem-type-opts)))
    (dotimes [_ (. stream readShort)]
      (let [key (. stream readShort)]
        (assoc! tmp key (val-apply-diff elem-type stream (get old key) elem-type-opts))))
    (persistent! tmp)))

;; Etc.

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
                      (bit-or 0
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
;; int map diffs - done

;; interpolation
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



(def-net-struct Foo :net [(x int)])

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      from {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3), 4 (Foo. 4)}
      from-client {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3), 4 (FooClient. 4)}
      to (-> from (dissoc 2) (assoc 5 (Foo. 5)) (assoc 1 (Foo. 777)))]
  (write-diff 'short-map dos from to {:elem-type 'Foo})
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (apply-diff 'short-map dis from-client {:elem-type 'Foo})))

(let [bos (new ByteArrayOutputStream)
      dos (new DataOutputStream bos)
      m {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3), 4 (Foo. 4)}]
  (write-bin 'short-map dos m {:elem-type 'Foo})
  (let [arr (. bos toByteArray)
        bis (new ByteArrayInputStream arr)
        dis (new DataInputStream bis)]
    (read-bin 'short-map dis {:elem-type 'Foo})))
