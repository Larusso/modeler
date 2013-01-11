(ns modler.core
  (:import (java.io ByteArrayInputStream File))
  (:require [clojure.xml :as xml]
            [clojure.string :refer (split join)]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zf]
            [modler.typeUtil :as typeUtil]
            [clojure.pprint :refer :all ]
            [clojure.java.io :refer :all ]
            [clostache.parser :as clostache] :verbose ))

(defn getClassObject
  "creates class object for generator"
  [classModel]
  (println "map class:")
  (= :iface (:tag classModel)))

(defn generateClass [model, tagType]
  (pprint (map typeUtil/get-class (typeUtil/filterTag tagType model)))
  (let [classes (map typeUtil/get-class (typeUtil/filterTag tagType model))]
    (doseq [item classes]
      (println (clostache/render-resource "templates/as3/class.mustache" item)))
    )
  )

(defn load-model
  [file-path]
  (typeUtil/get-struct-map (slurp file-path))
  )

(defn get-classes
  "returns a list with all the classes in the model"
  ([model lang]
    (binding [typeUtil/*lang* lang typeUtil/*model* model]
      (map typeUtil/get-class (typeUtil/filterTag :class (:content model)))
      )
    )
  ([model]
    (get-classes model "*")
    )
  )

(defn get-interfaces
  "returns a list with all the interfaces in the model"
  [model lang]
  (binding [typeUtil/*lang* lang typeUtil/*model* model]
    (map typeUtil/get-class (typeUtil/filterTag :iface (:content model)))
    )
  )

(defn get-template-name
  ([type]
    (format "%s.mustache" type))
  ([type lang]
    (if (= "*" lang)
      (get-template-name type)
      (format "%s.%s.mustache" lang type)
      )
    )
  )

(defn generate-type-source
  ([model template-path lang]

    (let [template-file-path (str template-path (get-template-name (:generate-type model) lang)) f (File. template-file-path)]
      (if (.isFile f)
        (do
          (map #(do (println %1) (merge {:source (clojure.string/replace %1 #"^\!\[(.*?)\]\s" "")}
                                   (load-string ((re-find #"^\!\[(.*?)\]\s" %1) 1))))
            (split
              (clostache/render (slurp (str template-path (get-template-name (:generate-type model) lang))) model)
              #"\#\[FILE_BREAK\]\s")
            ))
        (throw (Exception. (format "Error: Missing template: %s" template-file-path)))
        )
      )
    )
  ([model template-path]
    (generate-type-source model template-path "*")
    )
  )

(defn get-type-source-file-path
  "returns a path for the specific extension"
  [model output-path extension]
  (let [path (join "/" (split (:package model) #"\."))]
    (str output-path path "/" (:type-name model) "." extension)
    )
  )

(defn saveSource
  [{model :model
    extension :extension
    source :source
    output-path :output-path}]

  (let [file-path (get-type-source-file-path model output-path extension)]
    (make-parents (file file-path))
    (spit file-path source)
    )
  )

(defn generate-source
  [{lang :lang
    types :types
    template-path :template-path
    output-path :output-path :as p}]

  (map #(do
          (let [source-data (generate-type-source %1 template-path lang)
                source (map (fn [source-object]
                              (merge source-object p {:model %1})
                              ) source-data
              )
                ]
            ;;source
            (map saveSource source)
            )
          ) types)
  )

(defn generate
  [{model-path :model-path
    langs :languages
    template-path :templatePath
    output-path :output}]
  (let [modelData (load-model model-path)]
    (println "generate" langs)
    (map generate-source
      (map
        #(do
           {:lang %1
            :template-path template-path
            :output-path output-path
            :types (concat (get-classes modelData %1)
                     (get-interfaces modelData %1)
                     )}
           )
        langs)
      )
    )
  )