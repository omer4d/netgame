;; **********************
;; * game_net_utils.clj *
;; **********************

(in-ns 'game)

(defn open-datagram-channel [port]
  (let [channel (DatagramChannel/open)]
    (.bind (.socket channel) (new InetSocketAddress port))
    (.configureBlocking channel false)))

(defn close-datagram-channel [^DatagramChannel channel]
  (.close channel))
