(defproject clj-snake "0.1.0-SNAPSHOT"
  :description "Simple Snake game"
  :url "https://github.com/andreasfrom/clj-snake"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.4"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :aot [clj-snake.core]
  :main clj-snake.core)
