(ns modler.runner
  (:gen-class )
  (:require [clojure.string :refer (split)]
            [clojure.tools.cli :refer (cli)]
            [modler.core :as modler] :verbose ))

(defn run
  [{verbose :verbose
    output :output
    lang :languages
    templatePath :templatePath
    :as options} model-file]

  (let [model (modler/load-model model-file)
        types-by-lang (map #(concat (modler/get-interfaces model %1) (modler/get-classes model %1)) lang)]

    )
  )

(defn -main "Application entry point" [& args]
  (comment Do app initialization here)
  (println "model generator!")

  (let [[options args banner] (cli args
    ["-lang" "--languages" "comma separated list of languages to generate example 'as3,java,objc'" :parse-fn #(split % #",|;|:") :default "*"]
    ["-t" "--templatePath" "Path to template files" :default "./templates/"]
    ["-o" "--output" "The output path" :default "./generated/"]
    ["-v" "--[no-]verbose" :default false]
    ["-h" "--help" "Show help" :default false :flag true])]

    (when (:help options)
      (println banner)
      (System/exit 0))

    (println options)
    (println args)
    (modler/generate (merge options {:model-path (first args)}))
    )
  )