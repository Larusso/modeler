(ns modler.core
  (:import (java.io ByteArrayInputStream File))
  (:require [clojure.xml :as xml]
            [clojure.string :refer (split join)]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zf]
            [modler.typeUtil :as typeUtil]
            [clojure.pprint :refer :all ]
            [clojure.java.io :refer :all ]
            [clostache.parser :as clostache]))

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

(defn get-include-file-path
  [include-path parent-file-path]
  (join "/" (conj (pop (split parent-file-path #"/")) include-path))
  )

(defn check-import-path
  [import-path load-chain]
  (if (some #{import-path} load-chain)
    nil
    import-path
    )
  )

(defn load-model
  ([file-path load-chain]
    (let [raw-model (typeUtil/get-struct-map (slurp file-path))
          get-model-includes (fn [model] (->> model
                                           (:content )
                                           (typeUtil/filterTag :include )
                                           (map #(check-import-path (get-include-file-path (:path (:attrs %1)) file-path) load-chain))
                                           (filter #(not (nil? %1)))
                                           )
        )
          imports (get-model-includes raw-model)]
      (if-not (empty? imports)
        {:tag (:tag raw-model)
         :attrs (:attrs raw-model)
         :content (->> imports
                    (mapcat #(:content (load-model %1 (conj load-chain file-path))))
                    (concat (:content raw-model))
                    (filter #(not= :include (:tag %1)))
                    (into [])
                    )
         }
        raw-model
        )
      )
    )
  ([file-path]
    (load-model file-path [])
    )
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
  ([model lang]
    (binding [typeUtil/*lang* lang typeUtil/*model* model]
      (map typeUtil/get-interface (typeUtil/filterTag :iface (:content model)))
      )
    )
  ([model]
    (get-interfaces model "*")
    )
  )

(defn get-types
  [model lang]
  (concat (get-classes model lang) (get-interfaces model lang))
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

    (let [template-file-path (str template-path (get-template-name (:template-id model) lang))
          f (File. template-file-path)
          reg #"^\!\[(.*?)\]\s"]
      (if (and (.exists f) (.isFile f))
        (map #(if (not (nil? (re-find reg %1)))
                (merge {:source (clojure.string/replace %1 reg "")}
                  (load-string ((re-find reg %1) 1)))
                {:error (format "Error: missing template header in template: %s" template-file-path)}
                )
          (split
            (clostache/render (slurp (str template-path (get-template-name (:template-id model) lang))) model)
            #"\s\#\[FILE_BREAK\]\s")
          )
        [{:error (format "Error: missing template: %s" template-file-path)}]
        )
      )
    )
  ([model template-path]
    (generate-type-source model template-path "*")
    )
  )

(defn get-type-source-file-path
  "returns a path for the specific extension"
  [{:keys [model extension lang output-path]}]
  (let [path (join "/" (split (:package model) #"\."))
        lang-path (if (not= lang "*") (str lang "/") "")]
    (str output-path lang-path path "/" (:type-name model) "." extension)
    )
  )

(defn saveSource
  [{source :source :as options}]
  (let [file-path (get-type-source-file-path options)]
    (try
      (do
        (make-parents (file file-path))
        (spit file-path source)
        {:generated file-path}
        )
      (catch Exception e
        {:error (format "Error saving file: %s" file-path)}
        )
      )
    )
  )

(defn containsError?
  [coll]
  (contains? coll :error )
  )

(defn generate-source
  [{:keys [lang template-path types output-path]
    :as options}]

  (mapcat #(do
             (let [source-data (generate-type-source %1 template-path lang)
                   error (filter containsError? source-data)
                   passed (filter (complement containsError?) source-data)
                   source (map (partial merge options {:model %1}) passed)
                   generated (map saveSource source)]
               (if-not (empty? error)
                 (concat generated error)
                 generated
                 )
               )
             ) types)
  )

(defn generate
  [{:keys [model-path languages template-path output-path] :as options}]
  (let [modelData (load-model model-path)
        types-by-lang (map #(merge options {:lang %1 :types (get-types modelData %1)})
      languages)]
    (map generate-source types-by-lang)
    )
  )