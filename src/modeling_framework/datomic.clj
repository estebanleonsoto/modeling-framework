(ns modeling-framework.datomic
  (:require [modeling-framework.spec :as m]))

(def datomic-types
  {::m/string  :db.type/string
   ::m/long    :db.type/long
   ::m/boolean :db.type/boolean
   ::m/instant :db.type/instant
   ::m/big-int :db.type/bigint
   ::m/float   :db.type/float
   ::m/double  :db.type/double
   ::m/big-dec :db.type/bigdec
   ::m/uuid    :db.type/uuid
   ::m/uri     :db.type/uri
   ::m/bytes   :db.type/bytes
   ::m/ref     :db.type/ref})

(def datomic-cardinality
  {::m/single   :db.cardinality/one
   ::m/multiple :db.cardinality/many})

(defn attribute-schema [{:keys [id cardinality persistence-type description label]}]
  {:db/ident       id
   :db/valueType   (datomic-types persistence-type)
   :db/cardinality (or (datomic-cardinality cardinality) :db.cardinality/one)
   :db/doc         (or description label)})

(defn entity-schema [entity-model]
  {(entity-model :id)
   (->> entity-model
        (:attributes)
        (map attribute-schema)
        (doall)
        (vec))})


(defn schema [model]
  {(model :id)
   (->> model
        (:entities)
        (map entity-schema)
        (doall)
        (vec))})
