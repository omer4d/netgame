(ns game
  (:require [netcore :refer [def-net-struct write-bin read-bin write-diff apply-diff defbin]])
  (:require [quil.core :as q])
  (:import [java.nio ByteBuffer channels.DatagramChannel]
           [java.net InetSocketAddress InetAddress]))

;(set! *warn-on-reflection* false)
;(set! *unchecked-math* false)

(load "game_util")
(load "game_gameplay")
(load "game_net_protocol")
(load "game_net_util")
(load "game_server")
(load "game_client")

;(start-server! 0.01)
;(start-client!)
;(stop-server!)