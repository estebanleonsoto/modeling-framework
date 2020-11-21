(ns modeling-framework.datomic
  (:require [modeling-framework.spec :as m]
            [clojure.spec.alpha :refer [valid? explain]]))

(def datomic-types
  {::m/keyword :db.type/keyword
   ::m/string  :db.type/string
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

(defn add-unique-identity [schema identifies]
  "if identifies is true, adds a :db/unique property to the attribute schema"
  (if identifies
    (assoc schema :db/unique :db.unique/identity)
    schema))

(defn add-component [schema persistence-type sub-entity]
  "if it is a sub-entity and persistence-type is ::m/ref it adds a :db/isComponent to the attribute,
  meaning the entity this attribute refers to is managed as part of the containing property keeping
  referential integrity"
  (if (and sub-entity (= ::m/ref persistence-type))
    (assoc schema :db/isComponent true)
    schema))

(defn attribute-schema [{:keys [id cardinality persistence-type description label identifies sub-entity]}]
  (-> {:db/ident       id
       :db/valueType   (datomic-types persistence-type)
       :db/cardinality (or (datomic-cardinality cardinality) :db.cardinality/one)
       :db/doc         (or description label)}
      (add-unique-identity identifies)
      (add-component persistence-type sub-entity)))


(defn entity-schema [entity-model]
  {(entity-model :id)
   (->> entity-model
        (:attributes)
        (map attribute-schema)
        (cons {:db/ident       ::m/entity-id
               :db/valueType   :db.type/keyword
               :db/cardinality :db.cardinality/one
               :db/doc         "This attribute identifies the entity definition this belongs to, can be understood as the entity's type."})
        (doall)
        (vec))})


(defn validate-model [model]
  "Performs a clojure spec validation of the model"
  (when (not (valid? ::m/model model))
    (throw (IllegalArgumentException. (str "Error in model: " (explain ::m/model model))))))

(defn schema [model]
  "converts the model to a collection of datomic schemas"
  (validate-model model)
  (->> model
       (:entities)
       (map entity-schema)
       (doall)
       (vec)))

(defn conformity-transaction-data [model]
  "Converts the model to a collection of schema transactions conforming to
   avescodes/conformity. This is mainly intended to be used in luminus projects"
  (validate-model model)
  (->> model
       (:entities)
       (map (fn [entity] [(:id entity)
                          {:txes
                           [(get (entity-schema entity) (:id entity))]}]))
       (vec)
       (into {})))