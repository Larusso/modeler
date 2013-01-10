(ns modler.core-test
  (:require [clojure.test :refer :all ]
            [modler.core :refer :all ]
            [clojure.pprint :refer :all ]
            [modler.typeUtil :as typeUtil]))

(quote
  (deftest test-load-model
    (testing "load test model"
      (is (not (nil? (load-model "test-resources/testModel.xml"))))
      )
    ))

(defn is-base-object?
  [item]
  (and
    (contains? item :generate-type )
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
    )
  )

(deftest test-get-classes
  (testing "get classes java only"
    (let [model (load-model "test-resources/testModel2.xml")
          classes (get-classes model "java")]
      (is (not (nil? classes)))
      (is (= 5 (count classes)))
      (is (true? (every? is-class-object? classes)))
      (is (true? (every? is-base-object? classes)))
      (is (true? (every? #(= (:generate-type %1) "class") classes)))
      )
    )

  (testing "get classes as3 only"
    (let [model (load-model "test-resources/testModel2.xml")
          classes (get-classes model "as3")]
      (is (not (nil? classes)))
      (is (= 4 (count classes)))
      (is (true? (every? is-class-object? classes)))
      (is (true? (every? is-base-object? classes)))
      (is (true? (every? #(= (:generate-type %1) "class") classes)))
      )
    )

  (testing "get classes no lang"
    (let [model (load-model "test-resources/testModel2.xml")
          classes (get-classes model)]
      (is (not (nil? classes)))
      (is (= 2 (count classes)))
      (is (true? (every? is-class-object? classes)))
      (is (true? (every? is-base-object? classes)))
      (is (true? (every? #(= (:generate-type %1) "class") classes)))
      )
    )
  )

(deftest test-get-interfaces
  (testing "get interfaces"
    (let [model (load-model "test-resources/testModel.xml")
          interfaces (get-interfaces model "as3")]
      (is (not (nil? interfaces)))
      (is (= 5 (count interfaces)))
      (is (true? (every? is-base-object? interfaces)))
      (is (true? (every? #(= (:generate-type %1) "iface") interfaces)))
      )
    )
  )

(deftest test-get-template-name
  (testing "get template name with lang value"

    (is (= "as3.class.mustache" (get-template-name "class" "as3")))
    (is (= "as3.iface.mustache" (get-template-name "iface" "as3")))

    (is (= "java.class.mustache" (get-template-name "class" "java")))
    (is (= "java.iface.mustache" (get-template-name "iface" "java")))
    )

  (testing "get template name without lang value"
    (is (= "class.mustache" (get-template-name "class")))
    (is (= "iface.mustache" (get-template-name "iface")))

    (is (= "class.mustache" (get-template-name "class" "*")))
    (is (= "iface.mustache" (get-template-name "iface" "*")))
    )
  )

(deftest test-generate-type
  (testing "generate type"
    (let [model (load-model "test-resources/test-model-generate-type.xml")
          classes (get-classes model)]
      (is (string? (generate-type (first classes) "templates/" "as3")))
      (is (= "test as3 class: NormalClass" (generate-type (first classes) "templates/" "as3")))
      (is (= "test java class: NormalClass" (generate-type (first classes) "templates/" "java")))
      (is (= "test class: NormalClass" (generate-type (first classes) "templates/")))
      )
    )
  )