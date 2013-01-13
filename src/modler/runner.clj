(ns modler.runner
  (:gen-class )
  (:require [clojure.string :refer (split)]
            [clojure.tools.cli :refer (cli)]
            [modler.core :as modler]))

(defn print-result
  [result langs verbose]

  (map
    (fn [result lang]
      (println (format "----------------- %s --------------------" lang))
      (when (true? verbose)
        (doseq [result-obj result]
          (println (:file result-obj) (if (true? (:status result-obj)) "ok" "fault"))
          )
        (println "---------------------------------------------------")
        )
      (println (format "Entities created: %d ok: %d faults: %d"
                 (count result)
                 (count (filter #(true? (:status %1)) result))
                 (count (filter #(false? (:status %1)) result))
                 )
        )
      )
    result langs)
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

    (try
      (print-result (modler/generate (merge options {:model-path (first args)})) (:languages options) (:verbose options))
      (catch Exception e
        (println "ERROR: " (.getMessage e))
        )
      )
    )
  )