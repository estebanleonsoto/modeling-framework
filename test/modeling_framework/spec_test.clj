(ns modeling-framework.spec-test
  (:require [clojure.test :refer :all]
            [modeling-framework.spec :as m]
            [clojure.spec.alpha :as s])
  (:import (java.time Instant)
           (java.util UUID)
           (java.net URI)))

(deftest test-attribute-properties
  (testing
    "A persistence-type must be one of the allowed values"
    (is (s/valid? ::m/persistence-type ::m/string)))
  (testing
    "An id must be a qualified keyword"
    (is (s/valid? ::m/id ::something)))
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
    "A sub-entity field must be a qualified keyword"
    (is (s/valid? ::m/sub-entity ::my-keyword))
    (is (not (s/valid? ::m/sub-entity nil)))))

(def correct-default-value-test-cases
  [[::m/keyword :test-keyword]
   [::m/string "test-string"]
   [::m/long 100]
   [::m/boolean true]
   [::m/instant (. Instant now)]
   [::m/big-int (BigInteger/ONE)]
   [::m/big-dec (BigDecimal/TEN)]
   [::m/float (new Float 1.123)]
   [::m/double (new Double 4.13221)]
   [::m/uuid (UUID/randomUUID)]
   [::m/uri (new URI "http://clojuredocs.org/")]
   [::m/bytes (byte-array 2137489)]
   [::m/ref :test-keyword]])

(def incorrect-default-value-test-cases
  [[::m/keyword "test-string"]
   [::m/string :test-keyword]
   [::m/long "test-string"]
   [::m/boolean 100]
   [::m/instant true]
   [::m/big-int (. Instant now)]
   [::m/big-dec (BigInteger/ONE)]
   [::m/float (BigDecimal/TEN)]
   [::m/double (new Float 1.123)]
   [::m/uuid (new Double 4.13221)]
   [::m/uri (UUID/randomUUID)]
   [::m/bytes (new URI "http://clojuredocs.org/")]
   [::m/ref (byte-array 2137489)]])

(def basic-attribute
  {:id               ::some-name
   :label            "Some name"
   :spec             :attribute-spec
   :persistence-type ::m/string
   :description      "Some description"})

(deftest test-attributes
  (testing
    "Attribute properties specific constraints"
    (is (s/valid? ::m/attribute
                  {:id               ::some-name
                   :label            "Some name"
                   :spec             :attribute-spec
                   :persistence-type ::m/string
                   :required         true
                   :identifies       true
                   :sub-entity       ::something}))
    (is (s/valid? ::m/attribute
                  {:id               ::some-name
                   :label            "Some name"
                   :spec             :attribute-spec
                   :persistence-type ::m/string
                   :description      "Some description"}))
    (is (not (s/valid? ::m/attribute
                       {:id    ::some-name
                        :label "Some name"})))
    (is (not (s/valid? ::m/attribute
                       {:id   ::some-name
                        :spec :attribute-spec})))
    (is (not (s/valid? ::m/attribute
                       {:id    ::some-name
                        :label :some-label
                        :spec  :attribute-spec})))
    (is (not (s/valid? ::m/attribute
                       {:label "Some name"
                        :spec  :attribute-spec}))))
  (testing
    "An attribute can have unexpected keys"
    (is (s/valid? ::m/attribute
                  {:id               ::some-name
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
                  {:id         ::test-entity
                   :attributes [{:id               ::test-attribute
                                 :label            "Test Attribute"
                                 :spec             :something
                                 :persistence-type ::m/string}]}))
    (is (not (s/valid? ::m/entity
                       {
                        :attributes [{:id    :test-attribute
                                      :label "Test Attribute"
                                      :spec  :something}]})))
    (not (s/valid? ::m/entity
                   {:id ::test-entity}))))

(deftest test-model
  (testing
    "A model must have at least 1 entity and and :id"
    (is (not (s/valid? ::m/model
                       {})))
    (is (s/valid? ::m/model
                  {:id       ::model-name
                   :entities [{:id         ::test-entity
                               :attributes [{:id               ::test-attribute
                                             :label            "Test Attribute"
                                             :spec             :something
                                             :persistence-type ::m/instant}
                                            {:id               ::test-attribute-mandatory
                                             :label            "Mandatory Attribute"
                                             :spec             :something
                                             :persistence-type ::m/string
                                             :required         true}]}]}))
    (is (not (s/valid? ::m/model
                       {:entities [{:id         ::test-entity
                                    :attributes [{:id               ::test-attribute
                                                  :label            "Test Attribute"
                                                  :spec             :something
                                                  :persistence-type ::m/instant}
                                                 {:id               ::test-attribute-mandatory
                                                  :label            "Mandatory Attribute"
                                                  :spec             :something
                                                  :persistence-type ::m/string
                                                  :required         true}]}]})))))

(deftest fetch-entity-model-test
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
      (is (= expected (m/fetch-entity-model model :test-entity))))
    (testing
      "Fetch specific entity model with wrong entity key returns nil"
      (is (nil? (m/fetch-entity-model model :wrong-entity-key)))
      (is (nil? (m/fetch-entity-model model nil))))))

(def entity-spec-test-model
  {:id       :model-name,
   :entities [{:id         ::test-entity,
               :attributes [{:id               ::test-attribute,
                             :label            "Test Attribute",
                             :spec             (s/def :modeling-framework.spec-test/something1 #(instance? Instant %))
                             :persistence-type :modeling-framework.spec/instant}
                            {:id               ::test-attribute-mandatory,
                             :label            "Mandatory Attribute",
                             :spec             (s/def :modeling-framework.spec-test/something2 (and string? #(= (count %) 2)))
                             :persistence-type ::m/string,
                             :required         true}
                            {:id               ::test-attribute-mandatory-2,
                             :label            "Mandatory Attribute 2",
                             :spec             (s/def :modeling-framework.spec-test/something3 string?)
                             :persistence-type :m/string
                             :required         true}]}]})

(deftest simple-entity-required-attributes-spec-test
  (do
    (m/register-attributes-required-specs entity-spec-test-model)
    (testing
      "A valid model must pass the spec"
      (is (s/valid?
            ::test-entity-with-all-required-attributes
            {::test-attribute             (Instant/now)
             ::test-attribute-mandatory   "Hi"
             ::test-attribute-mandatory-2 "Yes"})))
    (testing
      "An entity cannot pass the spec if it is missing a required field
      (::test-attribute-mandatory-2)"
      (is (not (s/valid? ::test-entity-with-all-required-attributes
                         {::test-attribute           (Instant/now)
                          ::test-attribute-mandatory "Hi"}))))
    (testing
      "An entity can pass the spec if it has an attribute with the wrong type,
      because this spec is not about that"
      (is (s/valid? ::test-entity-with-all-required-attributes
                    {::test-attribute             (Instant/now)
                     ::test-attribute-mandatory   "Hi"
                     ::test-attribute-mandatory-2 1})))
    (testing
      "An entity can pass the spec if an attribute has a value that does
       not pass the attribute's spec, because required attributes specs
       are not about that"
      (is (s/valid? ::test-entity-with-all-required-attributes
                    {::test-attribute             (Instant/now)
                     ::test-attribute-mandatory   "This value is too long and does not pass the spec"
                     ::test-attribute-mandatory-2 "Yes"})))))

(deftest sub-entity-test
  (testing
    "A sub-entity must point to an existing entity id"
    (let [_ (s/def ::reference keyword?)
          _ (s/def ::simple-string string?)
          valid-model {:id ::valid-test-model
                       :entities
                           [{:id ::referent-entity
                             :attributes
                                 [{:id               ::reference-attribute
                                   :label            "Reference"
                                   :persistence-type ::m/ref
                                   :spec             ::reference
                                   :sub-entity       ::referred-entity}]}
                            {:id ::referred-entity
                             :attributes
                                 [{:id               ::my-other-attribute
                                   :label            "other attribute"
                                   :persistence-type ::m/string
                                   :spec             ::simple-string}]}]}
          invalid-model {:id ::invalid-test-model
                         :entities
                             [{:id ::referent-entity
                               :attributes
                                   [{:id               ::reference-attribute
                                     :label            "Reference"
                                     :persistence-type ::m/ref
                                     :spec             ::reference
                                     :sub-entity       ::wrong-reference}]}
                              {:id ::referred-entity
                               :attributes
                                   [{:id               ::my-other-attribute
                                     :label            "other attribute"
                                     :persistence-type ::m/string
                                     :spec             ::simple-string}]}]}]

      (is (s/valid? ::m/model valid-model))
      (is (not (s/valid? ::m/model invalid-model))))))


(s/def ::simple-string string?)
(s/def ::vector-of-strings (s/and #(= (type %) (type []))
                                  (s/coll-of string?)))
(s/def ::simple-float float?)
(s/def ::telephone-number-spec pos-int?)
(s/def ::ref keyword?)
(s/def ::category #(contains?
                     #{:normal
                       :whole-seller
                       :compita} %))

(def contact-channel-types
  #{:home
    :mobile
    :work
    :fax})

(s/def ::contact-channel-type-spec
  (s/and keyword?
         #(contains? contact-channel-types %)))

(def email-regex #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")
(s/def ::e-mail-spec #(re-matches email-regex %))

(def test-required-attribute-spec-model
  {:id ::data-model
   :entities
       [{:id ::client
         :attributes
             [{:id               ::first-name
               :spec             ::simple-string
               :label            "First-name"
               :description      "The first name of the client"
               :persistence-type ::string}
              {:id               ::last-name
               :spec             ::vector-of-strings
               :label            "Last-name"
               :description      "The last names of the client"
               :persistence-type ::string
               :required         true}
              {:id               ::identification
               :spec             ::simple-string
               :label            "Identification"
               :description      "The identification of the client"
               :persistence-type ::string
               :required         true}]}]})

(deftest test-required-attribute-spec
  (let [test-client-good {::first-name     "John"
                          ::last-name      "Doe"
                          ::identification "457899ki23"}
        test-client-bad {::first-name "Mary"}]

    (do
      (m/register-attributes-required-specs test-required-attribute-spec-model)
      (testing
        "The spec for client model must have been created"
        (is (contains? (s/registry) ::client-with-all-required-attributes)))
      (testing
        "A good entity passes the all required attributes spec"
        (let [result (s/explain
                       ::client-with-all-required-attributes
                       test-client-good)]
          (is (nil? result))))
      (testing
        "A bad entity is rejected by the required attributes spec, the
         explanation mentions the field causing the issue"
        (let [result
              (s/explain-data
                ::client-with-all-required-attributes
                test-client-bad)]
          (is (clojure.string/ends-with?
                (->> result
                     ::s/problems
                     first
                     :pred
                     str)
                ":modeling-framework.spec-test/last-name))")))))))


(def sub-entity-multiple-valid-model
  {:id ::sub-entity-multiple-valid-model
   :entities
       [{:id ::client
         :attributes
             [{:id               ::identification
               :label            "id"
               :persistence-type ::m/string
               :required         true}
              {:id          ::telephones
               :label       "telephones"
               :sub-entity  ::telephone-data
               :cardinality ::m/multiple}]}
        {:id ::telephone-data
         :attributes
             [{:id    ::telephone-number
               :label "number"
               :spec  ::telephone-number-spec}]}]})

(deftest
  test-spec-validation-of-sub-entities-with-cardinality-multiple
  (testing
    "When a sub-entity has :cardinality :multiple, the spec of has-all-attributes-valid should be applied with an s/every over the whole collection"
    (let [good-entity {::identification "123"
                       ::telephones     [{::telephone-number 12341254}]}
          sub-entity-not-in-collection {::identification "123"
                                        ::telephones     {::telephone-number 12341254}}
          sub-entity-with-invalid-value-in-collection {::identification "123"
                                                       ::telephones     [{::telephone-number "a"}]}]
      (m/register-attributes-valid-specs sub-entity-multiple-valid-model)
      (is (s/valid? ::client-with-attributes-valid good-entity))
      (is (not (s/valid? ::client-with-attributes-valid sub-entity-not-in-collection)))
      (is (not (s/valid? ::client-with-attributes-valid sub-entity-with-invalid-value-in-collection))))))


(def sub-entity-single-valid-model
  {:id ::sub-entity-single-valid-model
   :entities
       [{:id ::customer
         :attributes
             [{:id               ::identification
               :label            "id"
               :persistence-type ::m/string
               :required         true}
              {:id          ::telephones
               :label       "telephones"
               :sub-entity  ::telephone-data
               :cardinality ::m/single}]}
        {:id ::telephone-data
         :attributes
             [{:id    ::telephone-number
               :label "number"
               :spec  ::telephone-number-spec}]}]})

(deftest
  test-spec-validation-of-sub-entities-with-cardinality-single
  (testing
    "When a sub-entity has :cardinality :single, the spec of has-all-attributes-valid should be applied directly to the value"
    (let [good-entity {::identification "123"
                       ::telephones     {::telephone-number 12341254}}
          sub-entity-in-collection {::identification "123"
                                    ::telephones     [{::telephone-number 12341254}]}
          sub-entity-with-invalid-value-in-telephones {::identification "123"
                                                       ::telephones     {::telephone-number "a"}}]
      (m/register-attributes-valid-specs sub-entity-single-valid-model)
      (is (s/valid? ::customer-with-attributes-valid good-entity))
      (is (not (s/valid? ::customer-with-attributes-valid sub-entity-in-collection)))
      (is (not (s/valid? ::customer-with-attributes-valid sub-entity-with-invalid-value-in-telephones))))))


(def sub-entity-single-and-multiple-cardinality-required-model
  {:id ::sub-entity-single-and-multiple-cardinality-model
   :entities
       [{:id ::main-entity
         :attributes
             [{:id               ::sub-entity-single-entry
               :label            "Sub-entity-single"
               :persistence-type ::m/ref
               :sub-entity       ::sub-entity-single-entity
               :required         true}
              {:id               ::sub-entity-multiple-entry
               :label            "Sub-entity-multiple"
               :persistence-type ::m/ref
               :sub-entity       ::sub-entity-multiple-entity
               :cardinality      ::m/multiple
               :required         true}]}
        {:id ::sub-entity-single-entity
         :attributes
             [{:id               ::sub-entity-single-value
               :label            "Single"
               :persistence-type ::m/string
               :required         true}]}
        {:id ::sub-entity-multiple-entity
         :attributes
             [{:id               ::sub-entity-multiple-value
               :label            "Multiple"
               :persistence-type ::m/string
               :required         true}]}]})

(deftest sub-entity-multiple-cardinality-test
  (let [good-entity {::sub-entity-single-entry   {::sub-entity-single-value "single!"}
                     ::sub-entity-multiple-entry [{::sub-entity-multiple-value "multiple 1"}
                                                  {::sub-entity-multiple-value "multiple 2"}]}
        missing-element-in-sub-entity-single {::sub-entity-single-entry   {::this-is-a-wrong-key "single!"}
                                              ::sub-entity-multiple-entry [{::sub-entity-multiple-value "multiple 1"}
                                                                           {::sub-entity-multiple-value "multiple 2"}]}]
    (m/register-attributes-required-specs sub-entity-single-and-multiple-cardinality-required-model)
    (testing
      "If sub-entity has cardinality multiple, the required fields spec must be applied
      on the elements of the collection (and not on the the collection)"
      ;(clojure.pprint/pprint(s/explain-data :modeling-framework.spec-test/main-entity-with-all-required-attributes good-entity))
      (is (s/valid? :modeling-framework.spec-test/main-entity-with-all-required-attributes good-entity))
      (is (not (s/valid? :modeling-framework.spec-test/main-entity-with-all-required-attributes missing-element-in-sub-entity-single))))))


(def mandatory-spec-recursive-model {:id ::sub-entity-test-model
                                     :entities
                                         [{:id ::main-entity-rec-test
                                           :attributes
                                               [{:id               ::entity-id
                                                 :label            "Id"
                                                 :persistence-type ::m/string}
                                                {:id               ::sub-entity-1-entry
                                                 :label            "Sub-entity-1"
                                                 :persistence-type ::m/ref
                                                 :sub-entity       ::sub-entity-1-rec-test
                                                 :required         true}
                                                {:id               ::sub-entity-2-entry
                                                 :label            "Sub-entity-2"
                                                 :persistence-type ::m/ref
                                                 :sub-entity       ::sub-entity-2-rec-test
                                                 :required         true}]}
                                          {:id ::sub-entity-1-rec-test
                                           :attributes
                                               [{:id               ::sub-entity-1-id
                                                 :label            "Sub Entity Id 1"
                                                 :persistence-type ::m/string
                                                 :required         true}]}
                                          {:id ::sub-entity-2-rec-test
                                           :attributes
                                               [{:id               ::sub-entity-2-id
                                                 :label            "Sub Entity Id 2"
                                                 :persistence-type ::m/string
                                                 :required         true}]}]})

(deftest mandatory-spec-recursive
  (testing
    "Mandatory attributes for nil model"
    (is (nil? (m/register-attributes-required-specs nil))))
  (testing
    "Spec validation must call validation for its sub-entity"
    (do
      (m/register-attributes-required-specs mandatory-spec-recursive-model)
      (let [good-value {::entity-id "test-entity-1"
                        ::sub-entity-1-entry
                                    {::sub-entity-1-id "sub-entity-1"}
                        ::sub-entity-2-entry
                                    {::sub-entity-2-id "sub-entity-2"}}
            bad-value {::entity-id "test-entity-2"
                       ::sub-entity-1-entry
                                   {::wrong-key 3}
                       ::sub-entity-2-entry
                                   {::sub-entity-2-id "sub-entity-2"}}
            sub-entity-missing {::entity-id "test-entity-2"
                                ::sub-entity-2-entry
                                            {::sub-entity-2-id "sub-entity-2"}}]
        (is (s/valid? ::main-entity-rec-test-with-all-required-attributes good-value))
        (is (not (s/valid? ::main-entity-rec-test-with-all-required-attributes bad-value)))
        (is (not (s/valid? ::main-entity-rec-test-with-all-required-attributes sub-entity-missing)))))))

(deftest test-attribute-correct-type-predicate
  (let [test-model {:id               ::last-name
                    :spec             ::vector-of-strings
                    :label            "Last-name"
                    :cardinality      ::m/multiple
                    :description      "The last names of the client"
                    :persistence-type ::m/string
                    :required         true}
        test-client-good {::last-name ["Doe" "Windsor"]}
        test-client-bad-content {::last-name [1 2]}
        test-client-bad-no-collection {::last-name "Doe"}
        correct-type? (m/attribute-correct-type-predicate test-model)]
    (testing
      "True for matching type inside a collection for attributes with cardinality multiple"
      (is (correct-type? test-client-good)))
    (testing
      "False if the collection is of a different type then persistence-type"
      (is (not (correct-type? test-client-bad-content))))
    (testing
      "False if value is not a collection"
      (is (not (correct-type? test-client-bad-no-collection))))))

(def test-model-with-required-multiple-attribute
  {:id       ::model-required-multiple
   :entities [{:id         ::test-entity
               :attributes [{:id               ::test-entity-id
                             :label            "id"
                             :persistence-type ::m/ref
                             :required         true}
                            {:id               ::test-attribute
                             :label            "test-attribute"
                             :persistence-type ::m/string
                             :cardinality      ::m/multiple
                             :required         true}]}]})

(deftest test-required-attribute-does-not-care-about-cardinality
  (let [good-entity {::test-entity-id ::good
                     ::test-attribute ["value"]}
        test-attribute-not-multiple {::test-entity-id ::bad
                                     ::test-attribute "value"}]
    (m/register-attributes-required-specs test-model-with-required-multiple-attribute)
    (is (s/valid? ::test-entity-with-all-required-attributes good-entity))
    (is (s/valid? ::test-entity-with-all-required-attributes test-attribute-not-multiple))))

(s/def ::positive-int (s/and int? pos?))

(def test-model-with-valid-multiple-attribute
  {:id       ::model-required-multiple
   :entities [{:id         ::test-entity
               :attributes [{:id               ::test-entity-id
                             :label            "id"
                             :persistence-type ::m/ref
                             :required         true}
                            {:id               ::test-attribute
                             :label            "test-attribute"
                             :spec             ::positive-int
                             :persistence-type ::m/long
                             :cardinality      ::m/multiple
                             :required         true}]}]})



(deftest test-valid-attribute-when-cardinality-is-multiple
  (let [good-entity {::test-entity-id ::good
                     ::test-attribute [1]}
        test-attribute-not-collection-in-value {::test-entity-id ::bad
                                                ::test-attribute 1}]
    (m/register-attributes-valid-specs test-model-with-valid-multiple-attribute)
    (is (s/valid? ::test-entity-with-attributes-valid good-entity))
    (is (not (s/valid? ::test-entity-with-attributes-valid test-attribute-not-collection-in-value)))))

(deftest test-valid-value-type-predicate
  (let [predicate (m/attribute-correct-type-predicate {:id               ::first-name
                                                       :spec             ::simple-string
                                                       :label            "First-name"
                                                       :description      "The first name of the client"
                                                       :persistence-type ::m/string})]
    (testing
      "Created predicate returns true if attribute has type specified in attribute model"
      (is (predicate {::first-name "John"})))
    (testing
      "Created predicate returns false if attribute has different type as specified in attribute model"
      (is (not (predicate {::first-name 2}))))))

(defn- exists-spec-with-key [k]
  (->> (s/registry)
       (keys)
       (some #(= % k))))

(def basic-types-test-model
  {:id ::main-model
   :entities
       [{:id ::client
         :attributes
             [{:id               ::first-name
               :spec             ::simple-string
               :label            "First-name"
               :description      "The first name of the client"
               :persistence-type ::m/string}]}]})

(deftest test-create-valid-type-values-spec-for-simple-primitives
  (let [good-entity {::first-name "John"}
        wrong-entity {::first-name 1}]
    (m/register-correct-types-specs basic-types-test-model)
    (testing
      "Creates always a spec"
      (is (exists-spec-with-key ::client-with-all-types-correct))
      (is (s/valid? ::client-with-all-types-correct good-entity))
      (is (not (s/valid? ::client-with-all-types-correct wrong-entity))))))

(def sub-entity-test-model
  {:id       ::sub-entity-test-model,
   :entities [{:id         ::main-entity-rec-test,
               :attributes [{:id               ::entity-id,
                             :label            "Id",
                             :persistence-type ::m/string}
                            {:id               ::sub-entity-entry,
                             :label            "Sub-entity",
                             :persistence-type ::m/ref,
                             :sub-entity       ::sub-entity-rec-test,
                             :required         true}
                            {:id               ::sub-entity-entry-2,
                             :label            "Sub-entity-2",
                             :persistence-type ::m/ref,
                             :sub-entity       ::sub-entity-rec-2-test,
                             :required         true}]}
              {:id         ::sub-entity-rec-test,
               :attributes [{:id               ::sub-entity-1-id,
                             :label            "Sub Entity 1 Id",
                             :persistence-type ::m/string,
                             :required         true}]}
              {:id         ::sub-entity-rec-2-test,
               :attributes [{:id               ::sub-entity-2-id,
                             :label            "Sub Entity 2 Id",
                             :persistence-type ::m/string,
                             :required         true}]}]})

(deftest test-model-load-specs
  (let [good-entity {::entity-id "a22"
                     ::sub-entity-entry
                                 {::sub-entity-1-id "s1Id"}
                     ::sub-entity-entry-2
                                 {::sub-entity-2-id "s2Id"}}
        bad-type-in-main-entity {::entity-id 22
                                 ::sub-entity-entry
                                             {::sub-entity-1-id "s1Id"}
                                 ::sub-entity-entry-2
                                             {::sub-entity-2-id "s2Id"}}
        bad-type-in-sub-entity-entity {::entity-id "a22"
                                       ::sub-entity-entry
                                                   {::sub-entity-1-id true}
                                       ::sub-entity-entry-2
                                                   {::sub-entity-2-id "s2Id"}}
        missing-required-sub-entity {::entity-id "a22"
                                     ::sub-entity-entry
                                                 {::sub-entity-1-id "s1Id"}}]
    (m/load-specs-for-model sub-entity-test-model)
    (is (s/valid? ::main-entity-rec-test-with-all-types-correct good-entity))
    (is (not (s/valid? ::main-entity-rec-test-with-all-types-correct bad-type-in-main-entity)))
    (is (not (s/valid? ::main-entity-rec-test-with-all-types-correct bad-type-in-sub-entity-entity)))
    (is (not (s/valid? ::main-entity-rec-test-with-all-required-attributes missing-required-sub-entity)))))


(s/def ::3-string (s/and string? #(= (count %) 3)))
(def non-required-with-spec-model
  {:id ::non-required-with-spec-model
   :entities
       [{:id ::non-required-with-spec-entity
         :attributes
             [{:id               ::entity-id
               :label            "Entity id"
               :persistence-type ::m/keyword
               :required         true}
              {:id               :attribute-not-required-with-spec
               :spec             ::3-string
               :label            "3 string"
               :persistence-type ::m/string
               :required         false}]}]})

(deftest test-wrong-spec-during-required-validation
  (testing
    "Spec issues should not show up in required attributes validation"
    (let [good-entity {::entity-id                        :lorem
                       ::attribute-not-required-with-spec "123"}
          wrong-spec-value {::entity-id                        :ipsum
                            ::attribute-not-required-with-spec "1"}
          only-id {::entity-id :dolor}]
      (m/register-attributes-required-specs non-required-with-spec-model)
      (is (s/valid? ::non-required-with-spec-entity-with-all-required-attributes good-entity))
      (is (s/valid? ::non-required-with-spec-entity-with-all-required-attributes wrong-spec-value))
      (is (s/valid? ::non-required-with-spec-entity-with-all-required-attributes only-id)))))

(def non-required-multiple-with-spec-model
  {:id ::non-required-multiple-with-spec-model
   :entities
       [{:id ::non-required-multiple-with-spec-entity
         :attributes
             [{:id               ::entity-id
               :label            "Entity id"
               :persistence-type ::m/keyword
               :required         true}
              {:id               ::attribute-not-required-with-spec
               :spec             ::3-string
               :label            "3 string"
               :persistence-type ::m/string
               :cardinality      ::m/multiple
               :required         false}]}]})

(deftest test-spec-of-multiple-during-required-validation
  (testing
    "Spec issues should not show up in required attributes validation for fields with cardinality multiple"
    (let [good-entity {::entity-id                        :lorem
                       ::attribute-not-required-with-spec ["123"]}
          value-not-collection {::entity-id                        :ipsum
                                ::attribute-not-required-with-spec "123"}]
      (m/register-attributes-required-specs non-required-multiple-with-spec-model)
      (is (s/valid? ::non-required-multiple-with-spec-entity-with-all-required-attributes good-entity))
      (is (s/valid? ::non-required-multiple-with-spec-entity-with-all-required-attributes value-not-collection)))))

(deftest test-spec-of-multiple-during-spec-validation
  (testing
    "Spec validation should be applied to elements of the collection if :cardinality is multiple and not to the collection"
    (let [good-entity {::entity-id                        :lorem
                       ::attribute-not-required-with-spec ["123"]}
          value-not-collection {::entity-id                        :ipsum
                                ::attribute-not-required-with-spec "123"}]
      (m/register-attributes-valid-specs non-required-multiple-with-spec-model)
      (is (s/valid? ::non-required-multiple-with-spec-entity-with-attributes-valid good-entity))
      (is (not (s/valid? ::non-required-multiple-with-spec-entity-with-attributes-valid value-not-collection))))))