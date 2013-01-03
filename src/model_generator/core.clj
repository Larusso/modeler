(ns model-generator.core
  (:import (java.io ByteArrayInputStream))
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zf]
            [model-generator.typeUtil :as typeUtil]
            [clojure.pprint :refer :all ]
            [clostache.parser :as clostache] :verbose ))

(defn get-struct-map [xml]
  (if-not (empty? xml)
    (let [stream (ByteArrayInputStream. (.getBytes (.trim xml)))]
      (xml/parse stream))))

(defn get-zippedUp [map]
  (zip/xml-zip map))

(defn loadAndZipXml
  "loads the given xml file and creates zip map"
  [model]
  (get-zippedUp (get-struct-map (slurp model))))

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