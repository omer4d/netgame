(ns netgame.game
  (:require [netgame.core :refer [def-net-struct write-bin read-bin write-diff apply-diff defbin]])
  (:require [quil.core :as q])
  (:import [java.nio ByteBuffer channels.DatagramChannel]
           [java.net InetSocketAddress InetAddress]))

;(set! *warn-on-reflection* false)
;(set! *unchecked-math* false)

(load "game_misc")
(load "game_engine")
(load "game_protocol")
(load "game_net_utils")
(load "game_server")
(load "game_client")

;(start-server! 0.01)
;(start-client!)
;(stop-server!)
