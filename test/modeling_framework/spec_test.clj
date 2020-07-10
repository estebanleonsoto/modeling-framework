(ns modeling-framework.spec-test
  (:require [clojure.test :refer :all]
            [modeling-framework.spec :as m]
            [clojure.spec.alpha :as s]))

(deftest test-attributes
  (testing
    "An id must be a keyword"
    (is (s/valid? ::m/id :something)))
  (testing
    "A label can be a string"
    (is (s/valid? ::m/label "something")))
  (testing
    "A label can be a i18n reference (a keyword)"
    (is (s/valid? ::m/label :k1/k2)))
  (testing
    "A spec must be a keyword for now"
    (is (s/valid? ::m/spec ::test-spec)))
  (testing
    "A description is just a string"
    (is (s/valid? ::m/description "a test description")))
  (testing
    "An attribute must have name, label and spec"
    (is (s/valid? ::m/attribute
                  {:id    :some-name
                   :label "Some name"
                   :type  :attribute-spec}))
    (is (s/valid? ::m/attribute
                  {:id          :some-name
                   :label       "Some name"
                   :type        :attribute-spec
                   :description "Some description"}))
    (is (not (s/valid? ::m/attribute
                       {:id    :some-name
                        :label "Some name"})))
    (is (not (s/valid? ::m/attribute
                       {:id   :some-name
                        :type :attribute-spec})))
    (is (not (s/valid? ::m/attribute
                       {:label "Some name"
                        :type  :attribute-spec}))))
  (testing
    "An attribute can have unexpected keys"
    (is (s/valid? ::m/attribute
                  {:id            :some-name
                   :label         "Some name"
                   :type          :attribute-spec
                   :arbitrary-key ""}))))

(deftest test-entity
  (testing
    "An entity must have a name and at least one attribute"
    (is (s/valid? ::m/entity
                  {:id         :test-entity
                   :attributes [{:id    :test-attribute
                                 :label "Test Attribute"
                                 :type  :something}]}))
    (is (not (s/valid? ::m/entity
                       {
                        :attributes [{:id    :test-attribute
                                      :label "Test Attribute"
                                      :type  :something}]})))
    (not (s/valid? ::m/entity
                   {:id :test-entity}))))

(deftest test-model
  (testing
    "A model must have at least 1 entity"
    (is (not (s/valid? ::m/model
                       {})))
    (is (s/valid? ::m/model
                  {:id       :model-name
                   :entities [{:id         :test-entity
                               :attributes [{:id    :test-attribute
                                             :label "Test Attribute"
                                             :type  :something}]}]}))))









