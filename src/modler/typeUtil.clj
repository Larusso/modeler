(ns modler.typeUtil
  (:import (java.io ByteArrayInputStream))
  (:require [clojure.string :refer (split join lower-case)]
            [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.data.zip.xml :as zf]
            [clojure.pprint :refer :all ]
            [clojure.set :refer (difference)] :verbose ))

;;move this function
(defn get-struct-map [xml]
  (if-not (empty? xml)
    (let [stream (ByteArrayInputStream. (.getBytes (.trim xml)))]
      (xml/parse stream))))

(def ^:dynamic *lang* "as3")
(def ^:dynamic *model*)

(def types (zip/xml-zip (get-struct-map (slurp "model/types.xml"))))

(defn filterTag
  "filter model for specifig tag"
  [tag, model]
  (filter #(= tag (:tag %1)) model)
  )

(defn pack-list
  [list]
  (if (> (count list) 0)
    {:first (first list)
     :rest (rest list)
     :all list
     :count (count list)}
    )
  )

(defn getTagByAttributeValue
  [model tagName attribute value]
  ;;(first (filter #(and (= tagName (:tag %1)) (= value (:name (:attrs %1)))) model))

  (let [zipModel (zip/xml-zip model)]
    (first (zf/xml1-> zipModel tagName (zf/attr= attribute value)))
    )
  )

(defn get-class-by-name
  "returns class xml object from model"
  [model name]
  ;;(println "get-class-by-name" name)
  ;;(pprint model)
  (getTagByAttributeValue model :class :type name)
  )

(defn get-iface-by-name
  "returns class xml object from model"
  [model name]
  ;;(println "get-iface-by-name" name)
  (getTagByAttributeValue model :iface :type name)
  )

(defn hasTag?
  [model tag]
  (> (count (filterTag tag (:content model))) 0)
  )

(defn getTagList
  [model tag factory]
  (if (hasTag? model tag)
    (map factory (filterTag tag (:content model)))
    )
  )

(defn hasAttribut?
  [model attribute]
  (if (contains? (:attrs model) attribute)
    (not= "" (attribute (:attrs model)))
    false
    )
  )

(defn getTypeComponents
  "returns type components classNamespace and className"
  ([type]
    (getTypeComponents type true))
  ([type lookupType]
    (if (re-find #"\." type)
      (do
        (let [nameComponents (split type #"\.")]
          {:namespace (join "." (pop nameComponents)),
           :name (peek nameComponents)}
          )
        )

      (if (and (= type (lower-case type)) (true? lookupType))
        (do
          (let [newType (zf/xml1-> types :type [(zf/attr= :name type)] :target [(zf/attr= :lang *lang*)] (zf/attr :value ))]
            ;;(println newType type *lang*)
            (getTypeComponents newType false)
            )
          )
        {:namespace ""
         :name type}
        )
      )
    )
  )

(defn get-type-name
  "returns the className of the given type"
  [{type :type}]
  (:name (getTypeComponents type))
  )

(defn get-type-namespace
  "returns the namespace of the given type"
  [{type :type}]
  (:namespace (getTypeComponents type))
  )

(defn get-qualified-name
  [{className :name classNamespace :namespace}]
  (if (= "" classNamespace)
    (str className)
    (join "." [classNamespace className])
    )
  )

;;superclass
(defn create-extends-object
  [extend-tag]
  (getTypeComponents (:type (:attrs extend-tag)))
  )

(defn get-extends-types
  "returns super class type of class model or nil"
  [model]
  (getTagList model :extends create-extends-object)

  )

(defn extends-type?
  "check if the class model contains a super class definitions"
  [model]
  (hasTag? model :extends )
  )

;;implements

(defn implements-iface?
  [model]
  (hasTag? model :implements )
  )

(defn create-iFace-object
  [iface-tag]
  (getTypeComponents (:iface (:attrs iface-tag)))
  )

(defn get-implemented-interfaces
  [model]
  (if (implements-iface? model)
    (getTagList model :implements create-iFace-object)
    []
    )
  )

;;get properties


(defn createPropertieObject
  "transforms the given propertie xml tag to a hashmap with name and type"
  [propertyTag]

  (let [zipModel (zip/xml-zip propertyTag)]
    {
      :name (:name (:attrs propertyTag))
      :type (getTypeComponents (:type (:attrs propertyTag)))
      }
    )
  )

(defn get-properties
  "returns a sequence of class properties"
  ([model]
    (get-properties model false)
    )
  ([model include-ancestors]
    (let [retrieve-function (if (= (:tag model) :class ) get-class-by-name get-iface-by-name)
          declarations (distinct (concat (if (implements-iface? model)
                                           (mapcat #(get-properties (get-iface-by-name *model* (get-qualified-name %)) true) (get-implemented-interfaces model))
                                           ) (getTagList model :property createPropertieObject)))

          extended-declarations (distinct (concat (if (extends-type? model)
                                                    (mapcat #(get-properties (retrieve-function *model* (get-qualified-name %)) true) (get-extends-types model))
                                                    ) []))
          ]

      (if include-ancestors
        (distinct (concat declarations extended-declarations))
        (into [] (difference (set declarations) (set extended-declarations)))
        )
      )
    )
  )

(defn properties?
  [model]
  (> (count (get-properties model)) 0)
  )
;;params

(defn params?
  [model]
  (hasTag? model :param )
  )

(defn createParamObject
  [paramTag]
  {:name (:name (:attrs paramTag))
   :type (getTypeComponents (:type (:attrs paramTag)))}
  )

(defn getParams
  [model]
  (getTagList model :param createParamObject)
  )

;;get methods


(defn createMethodObject
  "transforms the given method xml model to a hashmap"
  [methodTag]
  (merge {:name (:name (:attrs methodTag))
          :returns (getTypeComponents (:returns (:attrs methodTag)))}
    (if (params? methodTag)
      {:params (getParams methodTag)}
      {}
      )
    )
  )

(defn get-methods
  "returns a sequens of class methods"
  ([model]
    (get-methods model false)
    )
  ([model include-ancestors]
    (let [retrieve-function (if (= (:tag model) :class ) get-class-by-name get-iface-by-name)
          declarations (distinct (concat (if (implements-iface? model)
                                           (mapcat #(get-methods (get-iface-by-name *model* (get-qualified-name %)) true) (get-implemented-interfaces model))
                                           ) (getTagList model :method createMethodObject)))

          extended-declarations (distinct (concat (if (extends-type? model)
                                                    (mapcat #(get-methods (retrieve-function *model* (get-qualified-name %)) true) (get-extends-types model))
                                                    ) []))
          ]

      (if include-ancestors
        (distinct (concat declarations extended-declarations))
        (into [] (difference (set declarations) (set extended-declarations)))
        )
      )
    ))

(defn methods?
  "returns true if the given classmodel contains methods"
  [model]
  (> (count (get-methods model)) 0)
  )

;;get imports
(defn get-imports
  "returns a sequence with all types used in class or interface"
  (
    ;;if no predicate function is specified, use default function
    [model]
    (get-imports model #(not (= "" (:namespace %))))
    )
  (
    [model pred]
    (distinct
      (filter pred
        (concat
          (get-implemented-interfaces model)
          (get-extends-types model)
          (map :type (get-properties model))
          (map :returns (get-methods model))
          (mapcat #(map :type (:params %1)) (get-methods model))
          )
        )
      )
    )
  )

;;get class object
(defn get-base-object
  [model]
  {:type-name (get-type-name (:attrs model))
   :package (get-type-namespace (:attrs model))
   :super-types (pack-list (get-extends-types model))
   :super-types? (extends-type? model)

   :imports (get-imports model)

   :properties (pack-list (get-properties model))
   :properties? (properties? model)
   :methods (pack-list (get-methods model))
   :methods? (methods? model)}
  )

(defn get-interface
  "returnsa a interface object map"
  [model]
  (get-base-object model)
  )

(defn get-class
  "returns a class object map"
  [model]
  (merge
    (get-base-object model)
    {:implements? (implements-iface? model)
     :implements (pack-list (get-implemented-interfaces model))}
    )
  )
