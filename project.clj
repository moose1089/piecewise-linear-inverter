(defproject inverter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.trace "0.7.10"]]
  :main ^:skip-aot inverter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
