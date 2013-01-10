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
      (every? is-class-object? classes)
      (every? is-base-object? classes)
      )
    )

  (testing "get classes as3 only"
    (let [model (load-model "test-resources/testModel2.xml")
          classes (get-classes model "as3")]
      (is (not (nil? classes)))
      (is (= 4 (count classes)))
      (every? is-class-object? classes)
      (every? is-base-object? classes)
      )
    )
  )

(quote
  (deftest test-get-interfaces
    (testing "get interfaces"
      (let [model (load-model "test-resources/testModel.xml")]
        (is (not (nil? (get-interfaces model))))
        (is (= 5 (count (get-interfaces model))))
        (every? is-base-object? (get-interfaces model))
        )
      )
    )

  (deftest test-generate-type
    (testing "generate type"
      (let [model (load-model "test-resources/testModel.xml")
            classes (get-classes model "as3")]
        (string? (generate-type (first classes) "as3"))
        )
      )
    ))