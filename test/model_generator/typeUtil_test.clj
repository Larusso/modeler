(ns model_generator.typeUtil-test
  (:require [clojure.test :refer :all ]
            [model-generator.typeUtil :refer :all ]
            [clojure.pprint :refer :all ]))

(def xmlModel (get-struct-map (slurp "test-resources/testModel.xml")))

(defn isType?
  ([type className]
    (isType? type className "")
    )
  ([type className namespace]
    (and (= className (:className type)) (= namespace (:classNamespace type))))
  )

(defn isTypeDefinition?
  [item]
  (and (contains? item :className ) (contains? item :classNamespace ))
  )

(deftest test-xml-model
  (testing "test empty class"
    (let [classObject (getTagByAttributeValue xmlModel :class :type "SimpleClass")]
      (is (not (nil? classObject)))
      (is (= "SimpleClass" (:type (:attrs classObject))))
      (is (not (nil? (:content classObject))))
      (is (= :class (:tag classObject)))
      )
    )

  (testing "test interface"
    (let [ifaceObject (getTagByAttributeValue xmlModel :iface :type "ISimpleInterface")]
      (is (not (nil? ifaceObject)))
      (is (= "ISimpleInterface" (:type (:attrs ifaceObject))))
      (is (not (nil? (:content ifaceObject))))
      (is (= :iface (:tag ifaceObject)))
      )
    )

  (testing "test not existing tag"
    (let [ifaceObject (getTagByAttributeValue xmlModel :class :type "doesNotExist")]
      (is (nil? ifaceObject))
      )
    )
  )

(deftest test-get-qualified-name
  (testing "get qualified class name"
    (let [type (getTypeComponents "de.example.TestClass")
          type2 (getTypeComponents "TestClass")]
      (is (= "de.example.TestClass" (get-qualified-name type)))
      (is (= "TestClass" (get-qualified-name type2)))
      )
    )
  )

(deftest test-pack-list
  (testing "pack-list"
    (let [list [1 2 3 4 5 6] packed-list (pack-list list)]
      (is (not (nil? packed-list)))
      (is (= [2 3 4 5 6] (:rest packed-list)))
      (is (= 1 (:first packed-list)))
      (is (= list (:all packed-list)))
      (is (= 6 (:count packed-list)))
      (is (= (count list) (:count packed-list)))
      )
    )

  (testing "pack-list empty"
    (let [list [] packed-list (pack-list list)]
      (is (nil? packed-list))
      )
    )

  (testing "pack-list one item"
    (let [list [2] packed-list (pack-list list)]
      (is (not (nil? packed-list)))
      (is (= [] (:rest packed-list)))
      (is (= 1 (:count packed-list)))
      )
    )
  )


(deftest test-get-class-by-name
  (testing "test get simple class"
    (let [classObject (get-class-by-name xmlModel "SimpleClass")]
      (is (not (nil? classObject)))
      (is (= "SimpleClass" (:type (:attrs classObject))))
      (is (not (nil? (:content classObject))))
      (is (= :class (:tag classObject)))
      )
    )

  (testing "not existing class"
    (let [ifaceObject (get-class-by-name xmlModel "doesNotExist")]
      (is (nil? ifaceObject))
      )
    )
  )

(deftest test-get-iface-by-name
  (testing "test get simple class"
    (let [classObject (get-iface-by-name xmlModel "ISimpleInterface")]
      (is (not (nil? classObject)))
      (is (= "ISimpleInterface" (:type (:attrs classObject))))
      (is (not (nil? (:content classObject))))
      (is (= :iface (:tag classObject)))
      )
    )

  (testing "not existing iface"
    (let [ifaceObject (get-iface-by-name xmlModel "doesNotExist")]
      (is (nil? ifaceObject))
      )
    )
  )

(deftest test-type-to-typecomponents

  (testing "getTypeComponents"
    (let [type1 (getTypeComponents "UIComponent") type2 (getTypeComponents "mx.components.UIComponent")]
      (is (= "" (:classNamespace type1)))
      (is (= "UIComponent" (:className type1)))

      (is (= "mx.components" (:classNamespace type2)))
      (is (= "UIComponent" (:className type2)))
      )
    )
  )

(deftest test-get-class-name
  (testing "getClassName"

    (is (= "UIComponent" (getClassName {:type "UIComponent"})))

    (is (= "UIComponent" (getClassName {:type "mx.components.UIComponent"})))

    )
  )

(deftest test-get-class-namespace
  (testing "getClassNamespace"
    (is (= "" (getClassNamespace {:type "UIComponent"})))

    (is (= "mx.components" (getClassNamespace {:type "mx.components.UIComponent"})))

    )
  )

(deftest test-class-imports
  (testing "get imports with basic class"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "BaseClass")
            imports (get-imports class)]
        (is (= 2 (count imports)))
        (is (not (nil? (first imports))))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with methods and method params"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "MultimethodClass")
            imports (get-imports class)]
        (is (= 8 (count imports)))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with superclass and methods and method params"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "MultimethodClass2")
            imports (get-imports class)]
        (is (= 3 (count imports)))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with implemented interface, properties and methods and method params"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "SuperTestClass")
            imports (get-imports class)]
        (is (= 10 (count imports)))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  )

(deftest test-get-properties
  (testing "get properties without extends or implementations"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "SimpleClass")
            interface (get-iface-by-name xmlModel "IBaseClass")
            check-method #(and (map? %) (contains? % :name ) (contains? % :type ))]

        (is (true? (properties? class)))
        (is (not (nil? (get-properties class))))
        (is (= 1 (count (get-properties class))))

        (is (every? check-method (get-properties class)))

        (is (false? (properties? interface)))
        (is (= [] (get-properties interface)))
        )
      )
    )

  (testing "get properties with extends or implementations"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "BaseClass")
            class2 (get-class-by-name xmlModel "ExtendedClass")
            check-method #(and (map? %) (contains? % :name ) (contains? % :type ))]

        (is (true? (properties? class)))
        (is (not (nil? (get-properties class))))
        (is (= 2 (count (get-properties class))))

        (is (every? check-method (get-properties class)))

        (is (true? (properties? class2)))
        (is (not (nil? (get-properties class2))))
        (is (= 1 (count (get-properties class2))))
        (is (= 4 (count (get-properties class2 true))))

        (is (every? check-method (get-properties class2)))
        )
      )
    )

  (testing "get properties with library class as super class"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "ExtendsLibraryClass")
            check-method #(and (map? %) (contains? % :name ) (contains? % :type ))]

        (is (true? (properties? class)))
        (is (not (nil? (get-properties class))))
        (is (= 1 (count (get-properties class))))
        (is (every? check-method (get-properties class)))
        )
      )
    )
  )

(deftest test-implements-interfaces
  (testing "implements interfaces"
    (binding [*model* xmlModel]
      (let [class (get-class-by-name xmlModel "BaseClass")
            class2 (get-class-by-name xmlModel "SimpleClass")]

        (is (true? (implements-iface? class)))
        (is (not (nil? (get-implemented-interfaces class))))
        (is (= 1 (count (get-implemented-interfaces class))))

        (is (false? (implements-iface? class2)))
        (is (not (nil? (get-implemented-interfaces class2))))
        (is (= 0 (count (get-implemented-interfaces class2))))

        )
      )
    )
  )

(deftest test-super-type
  (testing "class with one extend"
    (let [class (get-class-by-name xmlModel "BaseClass")]

      (is (true? (extends-type? class)))
      (is (seq? (get-extends-types class)))
      (is (= 1 (count (get-extends-types class))))
      (is (isType? (first (get-extends-types class)) "SimpleClass"))
      )
    )

  (testing "class with no extends tag"
    (let [class (get-class-by-name xmlModel "SimpleClass")]

      (is (false? (extends-type? class)))
      (is (nil? (get-extends-types class)))
      )
    )

  (testing "interface with multiple extends tags"
    (let [class (get-iface-by-name xmlModel "IMultiInheritance")]

      (is (true? (extends-type? class)))
      (is (seq? (get-extends-types class)))
      (is (= 2 (count (get-extends-types class))))
      (is (isType? (first (get-extends-types class)) "ISimpleInterface"))
      (is (isType? (last (get-extends-types class)) "IExtendedClass"))
      )
    )
  )

(deftest test-get-methods
  (testing "methods"
    (binding [*model* xmlModel]
      (let [class1 (get-class-by-name xmlModel "BaseClass")
            class2 (get-class-by-name xmlModel "SimpleClass")
            class3 (get-class-by-name xmlModel "ExtendedClass")
            class4 (get-class-by-name xmlModel "ExtendedClass2")
            class5 (get-class-by-name xmlModel "SuperTestClass")
            check-method #(and (map? %) (contains? % :name ) (contains? % :returns ))
            ]

        (is (true? (methods? class1)))
        (is (= (count (get-methods class1)) 3))

        (is (every? check-method (get-methods class1)))

        (is (false? (methods? class2)))
        (is (= [] (get-methods class2)))

        (is (true? (methods? class3)))
        (is (= (count (get-methods class3)) 1))

        (is (every? check-method (get-methods class3)))

        (is (true? (methods? class4)))
        (is (= (count (get-methods class4)) 1))

        (is (every? check-method (get-methods class4)))

        (is (true? (methods? class5)))
        (is (= (count (get-methods class5)) 4))
        (is (every? check-method (get-methods class5)))

        )
      )
    )
  )

(deftest getParamsTest
  (testing "params"
    (let [xmlObject {:tag :method
                     :attrs {:returns "int" :name "callMe2"}
                     :content [{:tag :param
                                :attrs {:type "int" :name "param1"}}
                               {:tag :param
                                :attrs {:type "string" :name "param2"}}
                               ]}]

      (is (true? (params? xmlObject)))
      (is (= 2 (count (getParams xmlObject))))
      (is (= "param1" (:name (first (getParams xmlObject)))))
      )
    )
  )

(deftest filterTagTest
  (testing "filter tag")
  (let [xmlObject {:tag :class,
                   :attrs {:type "de.tslarusso.model.ServerProfileInfo"},
                   :content [{:tag :method
                              :attrs {:returns "string" :name "callMe"}
                              :content nil}
                             {:tag :method
                              :attrs {:returns "int" :name "callMe2"}
                              :content nil}
                             {:tag :property
                              :attrs {:type "string" :name "name"},
                              :content nil}
                             {:tag :property
                              :attrs {:type "de.tslarusso.interfaces.IServerProfileInfo" :name "type"}
                              :content nil}
                             {:tag :property,
                              :attrs {:type "int" :name "count"}
                              :content nil}]}]

    (is (= 2 (count (filterTag :method (:content xmlObject)))))
    (is (= 3 (count (filterTag :property (:content xmlObject)))))
    )
  )

(deftest type-lookup
  (testing "lookup type as3"
    (binding [*lang* "as3"]
      (is (= "String" (:className (getTypeComponents "string"))))
      (is (= "int" (:className (getTypeComponents "int"))))
      (is (= "Number" (:className (getTypeComponents "long"))))
      (is (= "ArrayCollection" (:className (getTypeComponents "collection"))))
      (is (= {:classNamespace "mx.collections", :className "ArrayCollection"} (getTypeComponents "collection")))
      )
    )

  (testing "lookup type java"
    (binding [*lang* "java"]
      (is (= "String" (:className (getTypeComponents "string"))))
      (is (= "int" (:className (getTypeComponents "int"))))
      (is (= "long" (:className (getTypeComponents "long"))))
      (is (= "Collection<?>" (:className (getTypeComponents "collection"))))
      )
    )
  )

(deftest tag-list
  (testing "get tag list"
    (let [class (get-class-by-name xmlModel "BaseClass")
          list (getTagList class :property createPropertieObject)]
      (is (not= [] list))
      (is (= 2 (count list)))
      )
    )
  )