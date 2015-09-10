;; *******************
;; * game_server.clj *
;; *******************

(in-ns 'netgame.game)

(defrecord ServerState [channel buffer  clients game-state])

(defn create-server-state [port]
  (ServerState. (open-datagram-channel port) (ByteBuffer/allocate (* 1024 1024)) #{} (create-game 10)))

(defn update-server-state [{:keys [clients game-state] :as server-state} msgs dt]
  (let [msg-groups (group-by :type msgs)
        connecters (map :sockaddr (:cl-connecting msg-groups))
        disconnecters (into #{} (map :sockaddr (:cl-disconnecting msg-groups)))]
    (assoc server-state
           :clients (into #{} (concat
                               (filter #(not (contains? disconnecters %)) clients)
                               connecters))
           :game-state (update-game game-state dt))))

(defn server-tick [{:keys [buffer channel] :as old-state} dt]
  (let [msgs (read-msgs (:channel old-state) (:buffer old-state) parse-client-msg-body)
        {:keys [clients game-state] :as new-state} (update-server-state old-state msgs dt)]
    (when-not (empty? msgs)
      (println msgs)
      (println clients))
    (.clear buffer)
    (write-full-update-msg buffer game-state)
    (.flip buffer)
    (doseq [client clients]
      (.rewind buffer)
      (.send channel buffer client))
    new-state))

(def server-state (atom nil))
(def server-future (atom nil))

 (defn stop-server! []
  (future-cancel @server-future))

(defn start-server! [target-dt]
  (when-not (or (nil? @server-future) (future-done? @server-future))
    (stop-server!))
  (reset! server-state (create-server-state 9999))
  (reset! server-future
          (future (loop [last-tick-time 0]
                    (let [curr-time (System/nanoTime)]
                      (if (Thread/interrupted)
                        (close-datagram-channel (:channel @server-state))
                        (if (>= (nano->sec (- curr-time last-tick-time)) target-dt)
                          (do
                            (swap! server-state server-tick target-dt)
                            (recur curr-time))
                          (recur last-tick-time))))))))
