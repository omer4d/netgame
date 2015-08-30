(defproject netgame "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [quil "2.2.6"]]
  :repl-options {:init-ns netgame.core
                 :init (do (set! *warn-on-reflection* true)
                           (set! *unchecked-math* :warn-on-boxed))}
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"])
