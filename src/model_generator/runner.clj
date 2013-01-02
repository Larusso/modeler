(ns model-generator.runner
  (:gen-class )
  (:require [clojure.string :refer (split)]
            [clojure.tools.cli :refer (cli)]
            [model-generator.core :refer :all ] :verbose))

(defn -main "Application entry point" [& args]
  (comment Do app initialization here)
  (println "model generator!")

  (let [[options args banner] (cli args
    ["-lang" "--languages" "comma separated list of languages to generate example 'as3,java,c#,objc'" :parse-fn #(split % #",|;|:")]
    ["-t" "--templatePath" "Path to template files" :default "./templates/"]
    ["-o" "--output" "The output path" :default "./out/"]
    ["-v" "--[no-]verbose" :default false]
    ["-h" "--help" "Show help" :default false :flag true])]

    (when (:help options)
      (println banner)
      (System/exit 0))

    (println options)
    (println args)
    (loadAndZipXml (get args 0)))
  )