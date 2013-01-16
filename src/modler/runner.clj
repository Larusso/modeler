(ns modler.runner
  (:gen-class )
  (:require [clojure.string :refer (split)]
            [clojure.tools.cli :refer (cli)]
            [clojure.pprint :refer :all ]
            [modler.core :as modler]))

(defn containsError?
  [coll]
  (contains? coll :error )
  )

(defn print-result
  [results langs verbose]
  (dotimes [i (count results)]
    (let [result ((into [] results) i)
          lang ((into [] langs) i)
          errors (distinct (filter containsError? result))
          generated (filter (complement containsError?) result)]
      (println (format "----------------- %s --------------------" lang))
      (when (and (true? verbose) (not (empty? generated)))
        (doseq [result-obj generated]
          (println (:generated result-obj))
          )
        (println "---------------------------------------------------")
        )

      (println "")
      (println (format "Entities generated: %d errors: %d"
                 (count generated)
                 (count errors)
                 )
        )

      (when-not (empty? errors)
        (println "")
        (println "----------------- Errors --------------------")
        (doseq [error errors]
          (println (:error error))
          )
        )
      (println "")
      )
    )

  )

(defn -main "Application entry point" [& args]
  (comment Do app initialization here)
  (println "model generator!")

  (let [[options args banner] (cli args
    ["-lang" "--languages" "comma separated list of languages to generate example 'as3,java,objc'" :parse-fn #(split % #",|;|:") :default "*"]
    ["-t" "--template-path" "Path to template files" :default "./templates/"]
    ["-o" "--output-path" "The output path" :default "./generated/"]
    ["-v" "--[no-]verbose" :default false]
    ["-h" "--help" "Show help" :default false :flag true])]

    (when (:help options)
      (println banner)
      (System/exit 0))

    (print-result (modler/generate (merge options {:model-path (first args)})) (:languages options) (:verbose options))
    )
  )