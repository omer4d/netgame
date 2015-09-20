;; *********************
;; * game_protocol.clj *
;; *********************

(in-ns 'game)

(def msg-kws
  [:cl-connecting
   :cl-disconnecting
   :sv-full-update
   :sv-delta-update])

(def msg-kw->code-map (zipmap msg-kws (range)))
(def msg-code->kw-map (zipmap (range) msg-kws))

(defn msg-kw->code [kw]
  (kw msg-kw->code-map))

(defn msg-code->kw [code]
  (get msg-code->kw-map code))

(defbin MsgHeader
  (type-code byte))

(defn read-msgs [^DatagramChannel channel ^ByteBuffer buffer parse-body]
  (.clear buffer)
  (loop [msgs (transient [])
         sockaddr (.receive channel buffer)]
    (.flip buffer)
    (if (nil? sockaddr)
      (persistent! msgs)
      (let [{msg-type-code :type-code} (read-bin 'MsgHeader buffer nil)
            msg-type-kw (msg-code->kw msg-type-code)]
        (recur (conj! msgs {:type msg-type-kw
                            :content (parse-body buffer msg-type-kw)
                            :sockaddr sockaddr})
               (do (.clear buffer) (.receive channel buffer)))))))

(defn write-msg-header [buffer type-kw]
  (write-bin 'MsgHeader buffer (MsgHeader. (msg-kw->code type-kw)) nil))

(defn parse-client-msg-body [^ByteBuffer buffer msg-type-kw]
  (case msg-type-kw
    :cl-connecting nil
    :cl-disconnecting nil))

(defn parse-server-msg-body [^ByteBuffer buffer msg-type-kw]
  (case msg-type-kw
    :sv-full-update (read-bin 'Game buffer nil)))

(defn write-full-update-msg [buffer game-state]
  (write-msg-header buffer :sv-full-update)
  (write-bin 'Game buffer game-state nil))
