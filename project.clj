(defproject model_generator "0.1.0-SNAPSHOT"
  :description "Model class generator"
  :url "http://tslarusso.de/model_generator"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [de.ubercode.clostache/clostache "1.3.1"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.6"]]

  :profiles {:dev {:resource-paths ["test-resources"]}}
  :main model-generator.runner)
