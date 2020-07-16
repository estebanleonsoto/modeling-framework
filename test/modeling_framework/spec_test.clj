(ns modeling-framework.spec-test
  (:require [clojure.test :refer :all]
            [modeling-framework.spec :as m]
            [clojure.spec.alpha :as s])
  (:import (java.time Instant)))

(deftest test-attribute-properties
  (testing
    "A persistence-type must be one of the allowed values"
    (is (s/valid? ::m/persistence-type ::m/string)))
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
    "A required field must be boolean"
    (is (s/valid? ::m/required false))
    (is (s/valid? ::m/required true))
    (is (not (s/valid? ::m/required nil))))
  (testing
    "A identifies field must be boolean"
    (is (s/valid? ::m/identifies false))
    (is (s/valid? ::m/identifies true))
    (is (not (s/valid? ::m/identifies nil))))
  (testing
    "A sub-entity field must be boolean"
    (is (s/valid? ::m/sub-entity false))
    (is (s/valid? ::m/sub-entity true))
    (is (not (s/valid? ::m/sub-entity nil)))))

(def correct-default-value-test-cases
  [[::m/keyword :test-keyword]
   [::m/string  "test-string"]
   [::m/long    100]
   [::m/boolean true]
   [::m/instant (. Instant now)]
   [::m/big-int (BigInteger/ONE)]
   [::m/big-dec (BigDecimal/TEN)]
   [::m/float   (new Float 1.123)]
   [::m/double  (new Double 4.13221)]
   [::m/uuid    (java.util.UUID/randomUUID)]
   [::m/uri     (new java.net.URI "http://clojuredocs.org/")]
   [::m/bytes   (byte-array 2137489)]
   [::m/ref     :test-keyword]])

(def incorrect-default-value-test-cases
  [[::m/keyword "test-string"]
   [::m/string  :test-keyword]
   [::m/long    "test-string"]
   [::m/boolean 100]
   [::m/instant true]
   [::m/big-int (. Instant now)]
   [::m/big-dec (BigInteger/ONE)]
   [::m/float   (BigDecimal/TEN)]
   [::m/double  (new Float 1.123)]
   [::m/uuid    (new Double 4.13221)]
   [::m/uri     (java.util.UUID/randomUUID)]
   [::m/bytes   (new java.net.URI "http://clojuredocs.org/")]
   [::m/ref     (byte-array 2137489)]])

(def basic-attribute
  {:id               :some-name
   :label            "Some name"
   :spec             :attribute-spec
   :persistence-type ::m/string
   :description      "Some description"})

(deftest test-attributes
  (testing
    "Attribute properties specific constraints"
    (is (s/valid? ::m/attribute
                  {:id               :some-name
                   :label            "Some name"
                   :spec             :attribute-spec
                   :persistence-type ::m/string
                   :required         true
                   :identifies       true
                   :sub-entity       true}))
    (is (s/valid? ::m/attribute
                  {:id               :some-name
                   :label            "Some name"
                   :spec             :attribute-spec
                   :persistence-type ::m/string
                   :description      "Some description"}))
    (is (not (s/valid? ::m/attribute
                       {:id    :some-name
                        :label "Some name"})))
    (is (not (s/valid? ::m/attribute
                       {:id   :some-name
                        :spec :attribute-spec})))
    (is (not (s/valid? ::m/attribute
                       {:id    :some-name
                        :label :some-label
                        :spec  :attribute-spec})))
    (is (not (s/valid? ::m/attribute
                       {:label "Some name"
                        :spec  :attribute-spec}))))
  (testing
    "An attribute can have unexpected keys"
    (is (s/valid? ::m/attribute
                  {:id               :some-name
                   :label            "Some name"
                   :spec             :attribute-spec
                   :persistence-type ::m/long
                   :arbitrary-key    ""})))
  (testing
    "If :default-value property is present, it has to match :persistence-type"
    (is (->> correct-default-value-test-cases
             (map (fn [[type value]]
                    (assoc basic-attribute :persistence-type type
                                           :default-value value)))
             (map #(s/valid? ::m/attribute %))
             (every? true?))))
  (testing
    "If :default-value property is present, non matching values to :persistence-type make attribute invalid"
    (is (->> incorrect-default-value-test-cases
             (map (fn [[type value]]
                    (assoc basic-attribute :persistence-type type
                                           :default-value value)))
             (map #(s/valid? ::m/attribute %))
             (every? false?)))))



(deftest test-entity
  (testing
    "An entity must have a name and at least one attribute"
    (is (s/valid? ::m/entity
                  {:id         :test-entity
                   :attributes [{:id               :test-attribute
                                 :label            "Test Attribute"
                                 :spec             :something
                                 :persistence-type ::m/string}]}))
    (is (not (s/valid? ::m/entity
                       {
                        :attributes [{:id    :test-attribute
                                      :label "Test Attribute"
                                      :spec  :something}]})))
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
                               :attributes [{:id               :test-attribute
                                             :label            "Test Attribute"
                                             :spec             :something
                                             :persistence-type ::m/instant}]}]}))))

(deftest entity-model-test
  (let [model {:id       :model-name
               :entities [{:id         :test-entity
                           :attributes [{:id               :test-attribute
                                         :label            "Test Attribute"
                                         :spec             :something
                                         :persistence-type ::m/instant}]}]}
        expected {:id         :test-entity
                  :attributes [{:id               :test-attribute
                                :label            "Test Attribute"
                                :spec             :something
                                :persistence-type ::m/instant}]}]
    (testing
      "Fetch the model of a specific entity"
      (is (= expected (m/entity-model model :test-entity))))
    (testing
      "Fetch specific entity model with wrong entity key returns nil"
      (is (nil? (m/entity-model model :wrong-entity-key)))
      (is (nil? (m/entity-model model nil))))))