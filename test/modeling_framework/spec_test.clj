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
    "A model must have at least 1 entity"
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
                                             :required         true}]}]}))))

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

(deftest entity-spec-test
  (do
    (m/mandatory-attributes-model-spec entity-spec-test-model)
    (testing
      "A valid model must pass the spec"
      (is (s/valid?
            ::test-entity-with-all-required-attributes
            {::test-attribute             (Instant/now)
             ::test-attribute-mandatory   "Hi"
             ::test-attribute-mandatory-2 "Yes"})))
    (testing
      "An entity cannot pass the spec if it is missing a required field"
      (is (not (s/valid? ::test-entity-with-all-required-attributes
                         {:test-attribute           (Instant/now)
                          :test-attribute-mandatory "Hi"}))))
    (testing
      "An entity cannot pass the spec if it has an attribute with the wrong type"
      (is (not (s/valid? ::test-entity-with-all-required-attributes
                         {:test-attribute             (Instant/now)
                          :test-attribute-mandatory   "Hi",
                          :test-attribute-mandatory-2 2}))))
    (testing
      "An entity cannot pass the spec if an attribute has a value that does not pass the attribute's spec"
      (is (not (s/valid? ::test-entity-with-all-required-attributes
                         {:test-attribute             (Instant/now)
                          :test-attribute-mandatory   "This value is too long and does not pass the spec"
                          :test-attribute-mandatory-2 "Yes"}))))))

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
      (m/mandatory-attributes-entity-spec (m/fetch-entity-model test-required-attribute-spec-model ::client))
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
        "A bad entity is rejected by the required attributes spec"
        (let [result
              (s/explain-data
                ::client-with-all-required-attributes
                test-client-bad)]
          (is (not (nil? result))))))))

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


(deftest test-valid-type-with-attributes-cardinality-multiple-spec
  (let [test-model {:id ::client
                    :attributes
                        [{:id               ::last-name
                          :spec             ::vector-of-strings
                          :label            "Last-name"
                          :cardinality      ::m/multiple
                          :description      "The last names of the client"
                          :persistence-type ::m/string
                          :required         true}]}
        test-client-good {::last-name ["Doe" "Windsor"]}
        test-client-bad {::last-name "Doe"}
        spec-for-fields (m/valid-value-types-spec test-model)]
    (testing
      "A vector with values must pass the the valid types specs"
      (do
        (clojure.pprint/pprint (s/explain-data spec-for-fields test-client-good))
        (is (s/valid? spec-for-fields test-client-good))))
    (testing
      "A single value (not in a collection) cannot pass the valid types specs"
      (is (not (s/valid? spec-for-fields test-client-bad))))))

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

(deftest test-create-valid-type-values-spec-for-simple-primitives
  (let [model {:id ::client
               :attributes
                   [{:id               ::first-name
                     :spec             ::simple-string
                     :label            "First-name"
                     :description      "The first name of the client"
                     :persistence-type ::m/string}]}
        good-entity {::first-name "John"}
        wrong-entity {::first-name 1}
        valid-value-types-spec (m/valid-value-types-spec model)]
    (testing
      "Creates always a spec"
      (is (not (nil? valid-value-types-spec))))
    (testing
      "Entities with correct types pass spec"
      (is (s/valid? valid-value-types-spec good-entity)))
    (testing
      "Entities with wrong type fail the spec"
      (is (not (s/valid? valid-value-types-spec wrong-entity))))))

(deftest test-attributes-spec-to-entity-spec
  (let [model {:id ::client
               :attributes
                   [{:id               ::first-name
                     :spec             ::simple-string
                     :label            "First-name"
                     :description      "The first name of the client"
                     :persistence-type ::m/string}
                    {:id               ::last-name
                     :spec             ::vector-of-strings
                     :label            "Last-name"
                     :cardinality      ::m/multiple
                     :description      "The last names of the client"
                     :persistence-type ::m/string}
                    {:id               ::unspec-ed
                     :cardinality      ::m/multiple
                     :persistence-type ::m/string}]}
        test-entity-good {::first-name "John"
                          ::last-name  ["Doe" "Smith"]}
        test-entity-bad {::first-name 4
                         ::last-name  #{"Doe" "Smith"}}
        entity-attributes-spec (m/all-attributes-valid-spec model)]
    (is (not (nil? entity-attributes-spec)))
    (is (s/valid? entity-attributes-spec test-entity-good))
    (is (not (s/valid? entity-attributes-spec test-entity-bad)))))

(def mandatory-spec-recursive-model {:id ::sub-entity-test-model
                                     :entities
                                         [{:id ::main-entity-rec-test
                                           :attributes
                                               [{:id               ::entity-id
                                                 :label            "Id"
                                                 :persistence-type ::m/string}
                                                {:id               ::sub-entity-entry
                                                 :label            "Sub-entity"
                                                 :persistence-type ::m/ref
                                                 :sub-entity       ::sub-entity-rec-test
                                                 :required         true}]}
                                          {:id ::sub-entity-rec-test
                                           :attributes
                                               [{:id               ::sub-entity-id
                                                 :label            "Sub Entity Id"
                                                 :persistence-type ::m/string
                                                 :required         true}]}]})

(deftest mandatory-spec-recursive
  (testing
    "Spec validation must call validation for its sub-entity"
    (do
      (m/mandatory-attributes-model-spec mandatory-spec-recursive-model)
      (let [good-value {::entity-id "test-entity-1"
                        ::sub-entity-entry
                                    {::sub-entity-id "sub-entity-1"}}
            bad-value {::entity-id "test-entity-2"
                       ::sub-entity-entry
                                   {::wrong-key 3}}]
        (is (s/valid? ::main-entity-rec-test-with-all-required-attributes good-value))
        (is (not (s/valid? ::main-entity-rec-test-with-all-required-attributes bad-value)))))))


;(def test-model
;  {:id       :modeling-framework.spec/sub-entity-test-model,
;   :entities [{:id         :modeling-framework.spec/main-entity-rec-test,
;               :attributes [{:id               :modeling-framework.spec/entity-id,
;                             :label            "Id",
;                             :persistence-type :modeling-framework.spec/string}
;                            {:id               :modeling-framework.spec/sub-entity-entry,
;                             :label            "Sub-entity",
;                             :persistence-type :modeling-framework.spec/ref,
;                             :sub-entity       :modeling-framework.spec/sub-entity-rec-test,
;                             :required         true}
;                            {:id               :modeling-framework.spec/sub-entity-entry-2,
;                             :label            "Sub-entity-2",
;                             :persistence-type :modeling-framework.spec/ref,
;                             :sub-entity       :modeling-framework.spec/sub-entity-rec-2-test,
;                             :required         true}]}
;              {:id         :modeling-framework.spec/sub-entity-rec-test,
;               :attributes [{:id               :modeling-framework.spec/sub-entity-1-id,
;                             :label            "Sub Entity 1 Id",
;                             :persistence-type :modeling-framework.spec/string,
;                             :required         true}]}
;              {:id         :modeling-framework.spec/sub-entity-rec-2-test,
;               :attributes [{:id               :modeling-framework.spec/sub-entity-2-id,
;                             :label            "Sub Entity 2 Id",
;                             :persistence-type :modeling-framework.spec/string,
;                             :required         true}]}]})
;
;(deftest test-model-required-specs-declaration
;  (let [result ()]))

(def online-shop-model
  {:id ::data-model
   :entities
       [{:id ::client
         :attributes
             [{:id               ::first-name
               :spec             ::simple-string
               :label            "First-name"
               :description      "The first name of the client"
               :persistence-type ::m/string}
              {:id               ::last-name
               :spec             ::vector-of-strings
               :label            "Last-name"
               :cardinality      ::m/multiple
               :description      "The last names of the client"
               :persistence-type ::m/string}
              {:id               ::telephones
               :spec             ::ref
               :label            "Telephone"
               :cardinality      ::m/multiple
               :description      "The type of this telephone number"
               :persistence-type ::m/ref
               :sub-entity       ::telephone}
              {:id               ::emails
               :spec             ::ref
               :label            :email/emails
               :cardinality      ::m/multiple
               :description      "Email addresses of the client"
               :persistence-type ::m/ref
               :sub-entity       ::email}
              {:id               ::identification
               :spec             ::simple-string
               :label            "Id"
               :description      "The reference of the client's identification"
               :cardinality      ::m/single
               :persistence-type ::m/string
               :identifies       true}
              {:id               ::address
               :spec             ::ref
               :label            "Address"
               :description      "A reference to multiple addresses in an entity"
               :cardinality      ::m/multiple
               :persistence-type ::m/ref
               :sub-entity       ::address}
              {:id               ::category
               :spec             ::category
               :label            "Category"
               :description      "The category the client belongs to"
               :cardinality      ::m/multiple
               :persistence-type ::m/keyword}]}
        {:id ::telephone
         :attributes
             [{:id               ::telephone-number
               :persistence-type ::m/long
               :spec             ::telephone-number-spec
               :label            :telephone/number
               :description      "The telephone number"
               :required         true}
              {:id               ::telephone-type
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :telephone/type
               :description      "The type of telephone (mobile, home, work, etc."
               :required         true}]}
        {:id ::email
         :attributes
             [{:id               ::email-address
               :persistence-type ::m/string
               :spec             ::e-mail-spec
               :label            :email/email
               :description      "The email of the client"
               :required         true}
              {:id               ::email-type
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :email/type
               :description      "The type of email (mobile, home, work, etc."
               :required         false}]}
        {:id ::address
         :attributes
             [{:id               ::address-descriptor
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :address/type
               :description      "Describes the type of address this is. For example it can be house address office address delivery address billing address etc."
               :required         true}
              {:id               ::address-lines
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/line
               :cardinality      ::m/multiple
               :description      "Street and house number details of the address"}
              {:id               ::city
               :persistence-type ::m/string
               :label            :address/city
               :spec             ::simple-string
               :description      "The city or district of the address"}
              {:id               ::canton
               :label            :address/canton
               :spec             ::simple-string
               :persistence-type ::m/string
               :description      "The canton (or county) of the province"}
              {:id               ::state-or-province
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/state-or-province
               :description      "The state or province of the address"}
              {:id               ::postal-code
               :label            :address/postal-code
               :spec             ::simple-string
               :persistence-type ::m/string
               :description      "The postal code of the address"}
              {:id               ::country
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/country
               :description      "The country of the address"
               :required         true}
              {:id               ::coordinates
               :persistence-type ::m/ref
               :spec             ::re
               :label            :address/coordinates
               :description      "The global coordinates of the address in a map"
               :sub-entity       ::coordinates}]}
        {:id ::coordinates
         :attributes
             [{:id               ::latitude
               :persistence-type ::m/float
               :label            :address/latitude
               :spec             ::simple-float
               :description      "The latitude of the location in the map in decimal degrees"
               :required         true}
              {:id               ::longitude
               :persistence-type ::m/float
               :label            :address/longitude
               :spec             ::simple-float
               :description      "The longitude of the location in the map in decimal degrees"
               :required         true}]}]})

;(deftest evaluate-spec
;  (testing
;    "The client model must pass the spec"
;    (let [test-entity {::first-name     "Esteban"
;                       ::last-name      ["León" "Soto"]
;                       ::identification "24308978902341"
;                       ::telephones     [{::telephone-number 123784902318794321
;                                          ::telephone-type   :home}]}
;          ;::email          [{::email-address "esteban@email.com"
;          ;                   ::email-type    :home}]
;          ;::identification "1-456A-5456456"
;          ;::address        [{::address-descriptor :home
;          ;                   ::address-lines      ["De la farmacia La Paulina"
;          ;                                         "(que ya no existe)450 varas"
;          ;                                         "al este"]
;          ;                   ::city               "Paso de la Vaca"
;          ;                   ::canton             "San José"
;          ;                   ::state-or-province  "San José"
;          ;                   ::postal-code        "10101"
;          ;                   ::country            "Costa Rica"
;          ;                   ::coordinates        {::latitude  9.945511223885042
;          ;                                         ::longitude -84.12367698123887}}]
;          ;::category       :compita}
;          not-passing-test-entity {::first-name "John"}]
;      (m/mandatory-attributes-entity-spec (m/fetch-entity-model online-shop-model ::client))
;      (is (s/valid? ::client-with-all-required-attributes test-entity))
;      (is (not (s/valid? ::client-with-all-required-attributes not-passing-test-entity))))))