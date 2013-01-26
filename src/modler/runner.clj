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
  (with-out-str
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

  )

(defn -main "Application entry point" [& args]
  (comment Do app initialization here)
  (println "Welcome to modler\n")

  (let [[options args banner] (cli args
    ["-lang" "--languages" "comma separated list of languages to generate example 'as3,java,objc'" :parse-fn #(split % #",|;|:") :default ["*"]]
    ["-t" "--template-path" "Path to template files" :default "./templates/"]
    ["-o" "--output-path" "The output path" :default "./generated/"]
    ["-gm" "--generate-multiple" "generate multiple files" :default true :flag true]
    ["-gs" "--generate-single" "generate all source in one file" :default false :flag true]
    ["-v" "--[no-]verbose" :default false]
    ["-h" "--help" "Show help" :default false :flag true])
        usage (clojure.string/replace banner #"Usage:" "usage: modler [options] [model PATH]")]

    (when (or (:help options))
      (println usage)
      (System/exit 0))

    (when (and (false? (:generate-multiple options)) (false? (:generate-single options)))
      (println "please select -gm or -gs or both.")
      (println usage)
      (System/exit 0)
      )

    (try
      (println
        (print-result (modler/generate (merge options {:model-path (first args)})) (:languages options) (:verbose options))
        )
      (catch Exception e
        (println (.getMessage e) "\n")
        (println usage)
        (System/exit 1)
        )
      )
    )
  )