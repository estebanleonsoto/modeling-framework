(ns modeling-framework.datomic-test
  (:require [clojure.test :refer :all]
            [modeling-framework.datomic :refer :all]
            [modeling-framework.spec :as m]))

(def attributes-test-cases
  [[{:id               :test-attribute-id
     :schema           :some-schema
     :persistence-type ::m/long
     :cardinality      ::m/single
     :label            "Some nice label"}
    {:db/ident       :test-attribute-id
     :db/valueType   :db.type/long
     :db/cardinality :db.cardinality/one
     :db/doc         "Some nice label"}]

   [{:id               :test-attribute-id
     :schema           :some-schema
     :persistence-type ::m/string
     :cardinality      ::m/multiple
     :label            "Some nice label"}
    {:db/ident       :test-attribute-id
     :db/valueType   :db.type/string
     :db/cardinality :db.cardinality/many
     :db/doc         "Some nice label"}]])

(def type-mappings
  [[::m/keyword :db.type/keyword]
   [::m/string :db.type/string]
   [::m/long :db.type/long]
   [::m/boolean :db.type/boolean]
   [::m/big-int :db.type/bigint]
   [::m/float :db.type/float]
   [::m/double :db.type/double]
   [::m/big-dec :db.type/bigdec]
   [::m/ref :db.type/ref]
   [::m/instant :db.type/instant]
   [::m/uuid :db.type/uuid]
   [::m/uri :db.type/uri]
   [::m/bytes :db.type/bytes]])

(def all-types-mapping-test-cases
  (->> type-mappings
       (map (fn [[input expected]]
              [(assoc (get-in attributes-test-cases [0 0]) :persistence-type input)
               (assoc (get-in attributes-test-cases [0 1]) :db/valueType expected)]))
       (doall)))


(deftest test-attribute-schema
  (testing
    "Attributes must map all cardinality correctly"
    (->> attributes-test-cases
         (into all-types-mapping-test-cases)
         (map (fn [[input expected]]
                (let [result (attribute-schema input)]
                  (is (= expected result)))))
         (doall))))

(def test-entity-1
  {:id :test-entity-1
   :attributes
       [{:id               :att1
         :spec             :some-spec1
         :label            "A nice label"
         :description      "This is attribute 1 of test-entity-1"
         :persistence-type ::m/string}
        {:id               :att2
         :spec             :some-spec2
         :label            "A nice label 2"
         :description      "This is attribute 2 of test-entity-1"
         :cardinality      ::m/multiple
         :persistence-type ::m/float}]})

(deftest test-entity-schema
  (testing
    "All attributes in the entity must be mapped"
    (let [input test-entity-1
          expected {:test-entity-1
                    [{:db/ident       :att1
                      :db/valueType   :db.type/string
                      :db/cardinality :db.cardinality/one
                      :db/doc         "This is attribute 1 of test-entity-1"}
                     {:db/ident       :att2
                      :db/valueType   :db.type/float
                      :db/cardinality :db.cardinality/many
                      :db/doc         "This is attribute 2 of test-entity-1"}]}]
      (is (= expected (entity-schema input))))))

(def test-model
  {:id :model-1 :entities [test-entity-1]})

(deftest test-model-schema
  (testing
    "All entities of a model must be mapped"
    (let [input test-model
          expected {:model-1
                    [{:test-entity-1
                      [{:db/ident       :att1
                        :db/valueType   :db.type/string
                        :db/cardinality :db.cardinality/one
                        :db/doc         "This is attribute 1 of test-entity-1"}
                       {:db/ident       :att2
                        :db/valueType   :db.type/float
                        :db/cardinality :db.cardinality/many
                        :db/doc         "This is attribute 2 of test-entity-1"}]}]}]
      (is (= expected (schema input))))))



