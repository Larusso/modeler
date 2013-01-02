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

(defn createClassObject [xmltag]


  (merge
    {:className (typeUtil/getClassName (:attrs xmltag))}
    {:package (typeUtil/getClassNamespace (:attrs xmltag))}
    {:superClassTypes (typeUtil/pack-list (typeUtil/get-extends-types xmltag))}
    {:superClass? (typeUtil/extends-type? xmltag)}
    {:implements? (typeUtil/implements-iface? xmltag)}
    {:implements (typeUtil/pack-list (typeUtil/get-implemented-interfaces xmltag))}

    {:imports (typeUtil/get-imports xmltag)}

    {:properties (typeUtil/pack-list (typeUtil/get-properties xmltag))}
    {:properties? (typeUtil/properties? xmltag)}

    {:methods (typeUtil/pack-list (typeUtil/get-methods xmltag))}
    {:methods? (typeUtil/methods? xmltag)}
    )
  )

(defn generateClass [model, tagType]
  (pprint (map createClassObject (typeUtil/filterTag tagType model)))
  (let [classes (map createClassObject (typeUtil/filterTag tagType model))]
    (doseq [item classes]
      (println (clostache/render-resource "templates/as3/class.mustache" item)))
    )
  )