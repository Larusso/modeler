(ns modler.typeUtil
  (:import (java.io ByteArrayInputStream))
  (:import (javax.xml.parsers.SAXParserFactorySAXParserFactory))
  (:require [clojure.string :refer (split join lower-case split-lines)]
            [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.data.zip.xml :as zf]
            [clojure.data.zip :as dz]
            [clojure.pprint :refer :all ]
            [clojure.set :refer (difference)]))

(def ^:dynamic *lang* "*")
(def ^:dynamic *model*)

;;move this function
(defn get-struct-map [xml]
  (if-not (empty? xml)
    (let [stream (ByteArrayInputStream. (.getBytes (.trim xml)))]
      (xml/parse stream))))

(defn filterTag
  "filter model for specifig tag"
  [tag, model]
  (let [lang-value *lang*]
    (filter #(and (= tag (:tag %1))
               (if (contains? (:attrs %1) :lang )
                 (let [type-lang-value (:lang (:attrs %1))]
                   (or (= type-lang-value lang-value) (= type-lang-value "*"))
                   )
                 '(true)
                 )
               ) model)
    )

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
  (let [zipModel (zip/xml-zip model)]
    (first (zf/xml1-> zipModel tagName (zf/attr= attribute value)))
    )
  )

(defn get-class-by-name
  "returns class xml object from model"
  [model name]
  (getTagByAttributeValue model :class :type name)
  )

(defn get-iface-by-name
  "returns class xml object from model"
  [model name]
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

(defn get-types-definitions
  [model]
  (if (not (nil? model))
    (let [types (first (filterTag :types model))]
      (if (or (not (nil? types)) (not (empty? types)))
        (zip/xml-zip types)
        )
      )
    )
  )

(defn has-type-definition?
  [type]
  (let [types (get-types-definitions (:content *model*))]
    (if (nil? types)
      false
      (not (nil? (zf/xml1-> types :type [(zf/attr= :name type)])))
      )
    )
  )

(defn get-type-definition
  [type]
  (let [lang-value *lang*
        model-value *model*
        types (get-types-definitions (:content model-value))
        typeCheck #(zf/xml1-> types :type [(zf/attr= :name type)] :target [(zf/attr= :lang %1)] (zf/attr :value ))]
    (if (not (nil? (typeCheck lang-value)))
      (typeCheck lang-value)
      (typeCheck "*")
      )
    )
  )

(defn getTypeComponents
  "returns type components classNamespace and className"
  ([type]
    (getTypeComponents type true)
    )
  ([type lookupType]
    (if (re-find #"\." type)
      (do
        (let [nameComponents (split type #"\.")]
          {:namespace (join "." (pop nameComponents)),
           :name (peek nameComponents)}
          )
        )
      (do
        (if (and (true? lookupType)
              (= type (lower-case type))
              (bound? #'*lang*)
              (has-type-definition? type))
          (do
            (let [newType (get-type-definition type)]
              (if (nil? newType)
                (getTypeComponents type false)
                (getTypeComponents newType false)
                )
              )
            )
          (do
            {:namespace ""
             :name type}
            )
          )
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

;;/////////////////////////////////////////
;;  documentation
;;////////////////////////////////////////

;;todo move utility stuff in own namespace
(defn lang-attr=
  "Returns a query predicate that matches a node when its lang attribute has the value of lang-value or *"
  [lang-value]
  (fn [loc] (or ((zf/attr= :lang lang-value) loc) ((zf/attr= :lang "*") loc)))
  )

(defn text
  "Returns the textual contents of the given location, similar to
  xpaths's value-of"
  [loc]
  (apply str (zf/xml-> loc dz/descendants zip/node string?))
  )

(defn get-documentation
  [model]
  (let [lang-value *lang* zipped-model (zip/xml-zip model)
        doc-text (zf/xml1-> zipped-model :doc [(lang-attr= lang-value)] text)]
    (if-not (nil? doc-text) (map clojure.string/trim (split-lines doc-text)))
    )
  )

(defn get-annotations
  [model]
  (let [lang-value *lang* zipped-model (zip/xml-zip model)]
    (zf/xml-> zipped-model :annotate [(lang-attr= lang-value)] zf/text)
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
      :doc (get-documentation propertyTag)
      :docs? (not (nil? (get-documentation propertyTag)))
      :annotations (get-annotations propertyTag)
      }
    )
  )

(declare decorators? get-decorators)

(defn get-properties
  "returns a sequence of class properties"
  ([model]
    (get-properties model false)
    )
  ([model include-ancestors]
    (let [model-value *model* retrieve-function (if (= (:tag model) :class ) get-class-by-name get-iface-by-name)
          declarations (distinct (concat
                                   (if (implements-iface? model)
                                     (mapcat #(get-properties (get-iface-by-name model-value (get-qualified-name %)) true) (get-implemented-interfaces model))
                                     )
                                   (getTagList model :property createPropertieObject)))

          extended-declarations (distinct (concat (if (extends-type? model)
                                                    (mapcat #(get-properties (retrieve-function model-value (get-qualified-name %)) true) (get-extends-types model))
                                                    ) []))
          decorators (mapcat #(get-properties (get-class-by-name model-value (get-qualified-name %)) true) (map :type (get-decorators model)))
          ]
      (if include-ancestors
        (distinct (concat declarations extended-declarations decorators))
        (into [] (difference (set declarations) (set extended-declarations) (set decorators)))
        )
      )
    )
  )

(defn properties?
  [model]
  (> (count (get-properties model)) 0)
  )

;;const

(defn createConstObject
  [const-data]
  {
    :name (zf/xml1-> const-data (zf/attr :name ))
    :type (getTypeComponents (zf/xml1-> const-data (zf/attr :type )))
    :value (zf/xml1-> const-data zf/text)
    :doc (get-documentation const-data)
    :docs? (not (nil? (get-documentation const-data)))
    :annotations (get-annotations const-data)
    }
  )

(defn get-consts
  [model]
  (let [lang-value *lang* zipped-model (zip/xml-zip model)]
    (map createConstObject (zf/xml-> zipped-model :const [(lang-attr= lang-value)]))
    )
  )

(defn consts?
  [model]
  (> (count (get-consts model)) 0)
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
          :returns (getTypeComponents (:returns (:attrs methodTag)))
          :doc (get-documentation methodTag)
          :docs? (not (nil? (get-documentation methodTag)))
          :annotations (get-annotations methodTag)}
    (if (params? methodTag)
      {:params (pack-list (getParams methodTag))}
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
    (let [model-value *model* retrieve-function (if (= (:tag model) :class ) get-class-by-name get-iface-by-name)
          declarations (distinct (concat (if (implements-iface? model)
                                           (mapcat #(get-methods (get-iface-by-name model-value (get-qualified-name %)) true) (get-implemented-interfaces model))
                                           ) (getTagList model :method createMethodObject)))

          extended-declarations (distinct (concat (if (extends-type? model)
                                                    (mapcat #(get-methods (retrieve-function model-value (get-qualified-name %)) true) (get-extends-types model))
                                                    ) []))
          decorators (mapcat #(get-methods (get-class-by-name model-value (get-qualified-name %)) true) (map :type (get-decorators model)))
          ]

      (if include-ancestors
        (distinct (concat declarations extended-declarations decorators))
        (into [] (difference (set declarations) (set extended-declarations) (set decorators)))
        )
      )
    )
  )

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
          (map :type (get-decorators model))
          (get-implemented-interfaces model)
          (get-extends-types model)
          (map :type (get-properties model))
          (map :returns (get-methods model))
          (mapcat #(map :type (:all (:params %1))) (get-methods model))
          )
        )
      )
    )
  )

(defn create-decorator-object
  [decorates-tag]
  (let [model-value *model* decoratee (get-class-by-name model-value (:type (:attrs decorates-tag)))]
    {:type (getTypeComponents (:type (:attrs decorates-tag)))
     :decorator-name (:name (:attrs decorates-tag))
     :decorator-prefix (:prefix (:attrs decorates-tag))
     :properties? (or (properties? decoratee) (some :properties? (get-decorators decoratee)) false)
     :properties (distinct (concat (get-properties decoratee) (mapcat :properties (get-decorators decoratee))))
     :methods? (or (methods? decoratee) (some :methods? (get-decorators decoratee)) false)
     :methods (distinct (concat (get-methods decoratee) (mapcat :methods (get-decorators decoratee))))
     }
    )
  )

(defn get-decorators
  "returns a seqence of decorator objects"
  [model]
  (getTagList model :decorates create-decorator-object)
  )

(defn decorates?
  [model]
  (> (count (get-decorators model)) 0)
  )

;;get class object
(defn get-base-object
  [model]
  {:type-name (get-type-name (:attrs model))
   :package (get-type-namespace (:attrs model))
   :super-types (pack-list (get-extends-types model))
   :super-types? (extends-type? model)
   :template-id (:templateId (:attrs model))
   :imports (get-imports model)

   :properties (pack-list (get-properties model))
   :properties? (properties? model)
   :methods (pack-list (get-methods model))
   :methods? (methods? model)
   :doc (get-documentation model)
   :docs? (not (nil? (get-documentation model)))
   :annotations (get-annotations model)}
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
     :implements (pack-list (get-implemented-interfaces model))
     :consts? (consts? model)
     :consts (pack-list (get-consts model))
     :decorates? (decorates? model)
     :decorates (pack-list (get-decorators model))
     }
    )
  )
