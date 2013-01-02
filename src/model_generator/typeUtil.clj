(ns model-generator.typeUtil
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
          {:classNamespace (join "." (pop nameComponents)),
           :className (peek nameComponents)}
          )
        )

      (if (and (= type (lower-case type)) (true? lookupType))
        (do
          (let [newType (zf/xml1-> types :type [(zf/attr= :name type)] :target [(zf/attr= :lang *lang*)] (zf/attr :value ))]
            ;;(println newType type *lang*)
            (getTypeComponents newType false)
            )
          )
        {:classNamespace ""
         :className type}
        )
      )
    )
  )

(defn getClassName
  "returns the className of the given type"
  [{type :type}]
  (:className (getTypeComponents type))
  )

(defn getClassNamespace
  "returns the namespace of the given type"
  [{type :type}]
  (:classNamespace (getTypeComponents type))
  )

(defn get-qualified-name
  [{className :className classNamespace :classNamespace}]
  (if (= "" classNamespace)
    (str className)
    (join "." [classNamespace className])
    )
  )

;;get imports

(defn getImports
  "returns a sequence with all types used in class or interface"
  [model]
  (let [zipModel (zip/xml-zip model)]
    (distinct
      (filter #(not (= "" (:classNamespace %)))
        (concat
          (map getTypeComponents (zf/xml-> zipModel :implements (zf/attr :iface )))
          (map getTypeComponents (zf/xml-> zipModel :extends (zf/attr :type )))

          (map getTypeComponents (zf/xml-> zipModel :property (zf/attr :type )))
          (map getTypeComponents (zf/xml-> zipModel :method (zf/attr :returns )))
          (map getTypeComponents (zf/xml-> zipModel :method :param (zf/attr :type )))
          )
        )
      )
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

