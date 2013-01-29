(ns modler.typeUtil-test
  (:require [clojure.test :refer :all ]
            [modler.typeUtil :refer :all ]
            [clojure.pprint :refer :all ]))

(def xmlModel (get-struct-map (slurp "test-resources/testModel.xml")))
(def xmlModel2 (get-struct-map (slurp "test-resources/testModel2.xml")))
(def xmlModel3 (get-struct-map (slurp "test-resources/test-consts.xml")))

(defn isType?
  ([type className]
    (isType? type className "")
    )
  ([type className namespace]
    (and (= className (:name type)) (= namespace (:namespace type))))
  )

(defn isTypeDefinition?
  [item]
  (and (contains? item :name ) (contains? item :namespace ))
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

  (testing "class with namespace"
    (let [model (get-struct-map (slurp "test-resources/model/get_class_by_name/model.xml"))
          classObject (get-class-by-name model "de.example.NamespaceClass")]
      (is (not (nil? classObject)))
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
      (is (= "" (:namespace type1)))
      (is (= "UIComponent" (:name type1)))

      (is (= "mx.components" (:namespace type2)))
      (is (= "UIComponent" (:name type2)))
      )
    )
  )

(deftest test-get-type-name
  (testing "get-type-name"

    (is (= "UIComponent" (get-type-name {:type "UIComponent"})))

    (is (= "UIComponent" (get-type-name {:type "mx.components.UIComponent"})))

    )
  )

(deftest test-get-type-namespace
  (testing "get-type-namespace"
    (is (= "" (get-type-namespace {:type "UIComponent"})))

    (is (= "mx.components" (get-type-namespace {:type "mx.components.UIComponent"})))

    )
  )

(deftest test-class-imports
  (testing "get imports with basic class"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "BaseClass")
            imports (get-imports class)]
        (is (= 2 (count imports)))
        (is (not (nil? (first imports))))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with methods and method params"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "MultimethodClass")
            imports (get-imports class)]
        (is (= 8 (count imports)))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with superclass and methods and method params"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "MultimethodClass2")
            imports (get-imports class)]
        (is (= 3 (count imports)))
        (is (every? isTypeDefinition? imports))
        )
      )
    )

  (testing "get-imports class with implemented interface, properties and methods and method params"
    (binding [*model* xmlModel *lang* "as3"]
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
    (binding [*model* xmlModel *lang* "as3"]
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
    (binding [*model* xmlModel *lang* "as3"]
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
    (binding [*model* xmlModel *lang* "as3"]
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

(deftest test-get-consts
  (testing "get consts"
    (binding [*model* xmlModel3 *lang* "as3"]
      (let [class (get-class-by-name xmlModel3 "ConstClass")
            check-method #(and (map? %) (contains? % :name ) (contains? % :value ) (contains? % :type ))]

        (is (true? (consts? class)))
        (is (not (nil? (get-consts class))))
        (is (= 3 (count (get-consts class))))
        (is (every? check-method (get-consts class)))
        (is (= "CONST_1" (:value (first (get-consts class)))))
        )
      )
    )
  )

(deftest test-implements-interfaces
  (testing "implements interfaces"
    (binding [*model* xmlModel *lang* "as3"]
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
    (binding [*model* xmlModel *lang* "as3"]
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
        (is (= "copy" (:name (first (get-methods class3)))))

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
    (binding [*model* xmlModel *lang* "as3"]
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
  )

(defn is-base-object?
  [item]
  (and
    (contains? item :template-id )
    (contains? item :type-name )
    (contains? item :package )
    (contains? item :super-types )
    (contains? item :super-types? )
    (contains? item :imports )
    (contains? item :properties )
    (contains? item :properties? )
    (contains? item :methods )
    (contains? item :methods? )
    )
  )

(defn is-class-object?
  [item]
  (and
    (contains? item :implements )
    (contains? item :implements? )
    (contains? item :consts? )
    (contains? item :consts )
    (contains? item :decorates? )
    (contains? item :decorates )
    )
  )

(deftest test-get-base-object
  (testing "get base object with simple type"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "BaseClass")
            classMap (get-base-object class)]

        (is (map? classMap))
        (is (= (:type-name classMap) "BaseClass"))
        (is (= (:package classMap) ""))
        (is (is-base-object? classMap))
        )
      )
    )
  )

(deftest test-get-class
  (testing "get class object with simple type"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "BaseClass")
            classMap (get-class class)]

        (is (map? classMap))
        (is (is-class-object? classMap))
        (is (is-base-object? classMap))
        )
      )
    )
  )

(deftest test-get-interface
  (testing "get interface object with simple type"
    (binding [*model* xmlModel *lang* "as3"]
      (let [interface (get-iface-by-name xmlModel "IBaseClass")
            map (get-interface interface)]

        (is (map? map))
        (is (not (is-class-object? map)))
        (is (is-base-object? map))
        )
      )
    )
  )

(defn is-decorator-object?
  [item]
  (and
    (contains? item :type )
    (contains? item :decorator-name )
    (contains? item :properties? )
    (contains? item :properties )
    (contains? item :methods? )
    (contains? item :methods )
    )
  )

(deftest test-get-decorators
  (testing "get basic decorator"
    (binding [*model* (get-struct-map (slurp "test-resources/model/decorator/simple-class-decorator.xml")) *lang* "as3"]
      (let [class (get-class-by-name *model* "de.example.model.DecoratorClass")
            decorators (get-decorators class)
            decorator (first decorators)
            properties (get-properties class)
            methods (get-methods class)
            imports (get-imports class)
            ]
        (is (seq? decorators))
        (is (map? (first decorators)))
        (is (true? (is-decorator-object? (first decorators))))
        (is (= 2 (count (:properties decorator))))
        (is (true? (:properties? decorator)))
        (is (false? (:methods? decorator)))
        (is (empty? (:methods decorator)))
        (is (empty? methods))
        (is (= 1 (count properties)))
        (is (= 1 (count imports)))
        (is (= "decoratee" (:decorator-name decorator)))
        )
      )
    )

  (testing "get decorator with implemented interfaces"
    (binding [*model* (get-struct-map (slurp "test-resources/model/decorator/class-decorator-with-interface.xml")) *lang* "as3"]
      (let [class (get-class-by-name *model* "de.example.model.DecoratorClass")
            decorators (get-decorators class)
            decorator (first decorators)
            properties (get-properties class)
            methods (get-methods class)
            imports (get-imports class)]
        (is (seq? decorators))
        (is (map? (first decorators)))
        (is (true? (is-decorator-object? (first decorators))))
        (is (= 2 (count (:properties decorator))))
        (is (true? (:properties? decorator)))
        (is (true? (:methods? decorator)))
        (is (= 1 (count (:methods decorator))))
        (is (= 1 (count properties)))
        (is (= 0 (count methods)))
        )
      )
    )

  (testing "get decorators for class with decorations"
    (binding [*model* (get-struct-map (slurp "test-resources/model/decorator/class-decorator-class-with-decorator.xml")) *lang* "as3"]
      (let [class (get-class-by-name *model* "de.example.model.DecoratorClass")
            decorators (get-decorators class)
            decorator (first decorators)
            properties (get-properties class)
            methods (get-methods class)
            imports (get-imports class)]
        (is (seq? decorators))
        (is (map? (first decorators)))
        (is (true? (is-decorator-object? (first decorators))))

        (is (true? (:properties? decorator)))
        (is (= 3 (count (:properties decorator))))

        (is (true? (:methods? decorator)))
        (is (= 1 (count (:methods decorator))))

        (is (= 0 (count properties)))
        (is (= 0 (count methods)))
        )
      )
    )
  )

(deftest filterTagTest
  (testing "filter tag lang as3"
    (binding [*model* xmlModel2 *lang* "as3"]
      (is (= 4 (count (filterTag :class (:content xmlModel2)))))
      (is (= 0 (count (filterTag :iface (:content xmlModel2)))))

      (let [class (get-class-by-name xmlModel2 "NormalClass")]
        (is (= 3 (count (filterTag :method (:content class)))))
        )
      )
    )

  (testing "filter tag lang as3"
    (binding [*model* xmlModel2 *lang* "java"]
      (is (= 5 (count (filterTag :class (:content xmlModel2)))))
      (is (= 0 (count (filterTag :iface (:content xmlModel2)))))

      (let [class (get-class-by-name xmlModel2 "NormalClass")]
        (is (= 4 (count (filterTag :method (:content class)))))
        )
      )
    )

  (testing "parameter filtering as3"
    (binding [*model* (get-struct-map (slurp "test-resources/test-model-filter-tag-lang.xml"))
              *lang* "as3"]
      (let [class (get-class-by-name *model* "NormalClass")
            methods (filterTag :method (:content class))]
        (is (= 1 (count methods)))
        (is (= 3 (count (filterTag :param (:content (first methods))))))
        )
      )
    )

  (testing "parameter filtering java"
    (binding [*model* (get-struct-map (slurp "test-resources/test-model-filter-tag-lang.xml"))
              *lang* "java"]
      (let [class (get-class-by-name *model* "NormalClass")
            methods (filterTag :method (:content class))]
        (is (= 1 (count methods)))
        (is (= 4 (count (filterTag :param (:content (first methods))))))
        )
      )
    )
  )

(deftest type-lookup
  (testing "lookup type as3"
    (binding [*model* xmlModel *lang* "as3"]
      (is (= "String" (:name (getTypeComponents "string"))))
      (is (= "void" (:name (getTypeComponents "void"))))
      (is (= "Void" (:name (getTypeComponents "void2"))))
      (is (= "int" (:name (getTypeComponents "int"))))
      (is (= "Number" (:name (getTypeComponents "long"))))
      (is (= "ArrayCollection" (:name (getTypeComponents "collection"))))
      (is (= {:namespace "mx.collections", :name "ArrayCollection"} (getTypeComponents "collection")))
      )
    )

  (testing "lookup type java"
    (binding [*model* xmlModel *lang* "java"]
      (is (= "String" (:name (getTypeComponents "string"))))
      (is (= "int" (:name (getTypeComponents "int"))))
      (is (= "long" (:name (getTypeComponents "long"))))
      (is (= "Collection<?>" (:name (getTypeComponents "collection"))))
      (is (= "asyncMessage" (:name (getTypeComponents "asyncMessage"))))
      )
    )

  (testing "lookup type without lang"
    (binding [*model* xmlModel]
      (is (= "string" (:name (getTypeComponents "string"))))
      (is (= "void" (:name (getTypeComponents "void"))))
      (is (= "Void" (:name (getTypeComponents "void2"))))
      (is (= "int" (:name (getTypeComponents "int"))))
      (is (= "long" (:name (getTypeComponents "long"))))
      (is (= "collection" (:name (getTypeComponents "collection"))))
      )
    )
  )

(deftest tag-list
  (testing "get tag list"
    (binding [*model* xmlModel *lang* "as3"]
      (let [class (get-class-by-name xmlModel "BaseClass")
            list (getTagList class :property createPropertieObject)]
        (is (not= [] list))
        (is (= 2 (count list)))
        )
      )
    )
  )

(deftest test-get-types-definitions
  (testing "get types"
    (binding [*model* xmlModel]
      (is (not (nil? (get-types-definitions (:content *model*)))))
      )
    (is (nil? (get-types-definitions (:content *model*))))
    )

  (testing "has type definition?"
    (binding [*model* xmlModel]
      (is (true? (has-type-definition? "void")))
      (is (false? (has-type-definition? "typeThatIsNotThere")))
      )
    )

  (testing "get type definition with lang as3"
    (binding [*model* xmlModel *lang* "as3"]
      (is (not (nil? (get-type-definition "void"))))
      (is (= "void" (get-type-definition "void")))
      (is (= "Void" (get-type-definition "void2")))
      (is (= "Object" (get-type-definition "object")))
      )
    )

  (testing "get type definition with lang java"
    (binding [*model* xmlModel *lang* "java"]
      (is (not (nil? (get-type-definition "void"))))
      (is (= "void" (get-type-definition "void")))
      (is (= "Void" (get-type-definition "void2")))
      (is (= "Map" (get-type-definition "object")))
      )
    )
  )

