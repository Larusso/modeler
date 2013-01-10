(ns modler.core
  (:import (java.io ByteArrayInputStream))
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zf]
            [modler.typeUtil :as typeUtil]
            [clojure.pprint :refer :all ]
            [clostache.parser :as clostache] :verbose ))

(def templateNames {:class "class.mustache" :iface "interface.mustache"})

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
  [model lang]
  (binding [typeUtil/*lang* lang typeUtil/*model* model]
    (map typeUtil/get-class (typeUtil/filterTag :class (:content model)))
    )
  )

(defn get-interfaces
  "returns a list with all the interfaces in the model"
  [model lang]
  (binding [typeUtil/*lang* lang typeUtil/*model* model]
    (map typeUtil/get-class (typeUtil/filterTag :iface (:content model)))
    )
  )


(defn generate-type
  [model languages]
  )