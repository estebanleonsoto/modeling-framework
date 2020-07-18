(ns modeling-framework.spec
  "Spec of the data structures used for describing models"
  (:require [clojure.spec.alpha :as s :refer [coll-of]])
  (:import (clojure.lang Keyword)
           (java.time Instant)
           (java.util UUID)
           (java.net URI)))


(def persistence-types
  {::keyword Keyword
   ::string  String
   ::long    Long
   ::boolean Boolean
   ::instant Instant
   ::big-int BigInteger
   ::big-dec BigDecimal
   ::float   Float
   ::double  Double
   ::uuid    UUID
   ::uri     URI
   ::bytes   (type (byte-array 0xF0))
   ::ref     Keyword})

(def cardinality-values
  #{::single
    ::multiple})



(s/def ::i18n-ref keyword?)
(s/def ::id keyword?)
(s/def ::label (s/or :literal string? :keyword ::i18n-ref))
(s/def ::spec keyword?)
(s/def ::description string?)
(s/def ::persistence-type #(not (nil? (persistence-types %))))
(s/def ::cardinality #(contains? cardinality-values %))
(s/def ::required boolean?)
(s/def ::identifies boolean?)
(s/def ::sub-entity keyword?)
(s/def ::default-value #(not (nil? %)))


(s/def ::matching-default-value-and-persistence-type
  #(if (:default-value %)
     (let [default-value (:default-value %)
           attribute-type (persistence-types (:persistence-type %))]
       (and (not (nil? default-value))
            (= attribute-type (type default-value))))
     true))

(s/def ::attribute
  (s/and
    (s/keys :req-un [::id ::label ::spec ::persistence-type]
            :opt-un [::description ::cardinality ::required
                     ::identifies ::sub-entity ::default-value])
    ::matching-default-value-and-persistence-type))

(s/def ::attributes
  (coll-of ::attribute :min-count 1))

(s/def ::entity
  (s/keys :req-un [::id ::attributes]
          :opt-un [::description]))

(s/def ::entities
  (coll-of ::entity :min-count 1))


(s/def ::valid-sub-entity-ref
  (fn [model]
    (let [sub-entity-refs (->> model
                               (:entities)
                               (map :attributes)
                               (flatten)
                               (map :sub-entity)
                               (filter #(not (nil? %))))
          entity-ids (->> model
                          (:entities)
                          (map :id)
                          (set))]
      (every? #(contains? entity-ids %) sub-entity-refs))))

(s/def ::model
  (s/and
    (s/keys :req-un [::id
                     ::entities]
            :opt-un [::description])
    ::valid-sub-entity-ref))

(defn entity-model [model entity-key]
  (->> model
       (:entities)
       (filter #(= entity-key (:id %)))
       (first)))

(defn has-all-mandatory-fields?-provider [entity-model]
  (fn [entity]
    (let [mandatory-fields (->> entity-model
                                (:attributes)
                                (filter :required)
                                (map :id))]
      (->> mandatory-fields
           (map #(contains? entity %))
           (every? true?)))))

(defn all-fields-valid?-provider [entity-model]
  (let [field-to-spec (->> entity-model
                           (:attributes)
                           (map #(vector (:id %) (:spec %)))
                           (into {}))]
    (fn [entity]
      (->> entity
           (map #(s/valid? (field-to-spec (first %)) (last %)))
           (every? true?)))))

(defn all-fields-correct-types?-provider [entity-model]
  (let [field-to-type (->> entity-model
                           (:attributes)
                           (map #(vector (:id %) (persistence-types (:persistence-type %))))
                           (into {}))]
    (fn [entity]
      (->> entity
           (map #(= (field-to-type (first %)) (type (last %))))
           (every? true?)))))


(defn entity-spec [model entity-key]
  (when (not (qualified-keyword? entity-key))
    (throw (new IllegalArgumentException "Entity-key must be a qualified keyword")))
  (let [entity-model (entity-model model entity-key)
        mandatory-fields-spec-name (keyword
                                     (str entity-key
                                          "-mandatory-fields"))

        has-mandatory-fields-spec (s/def
                                    mandatory-fields-spec-name
                                    (has-all-mandatory-fields?-provider entity-model))

        valid-types-spec-name (keyword
                                (str entity-key
                                     "-types-valid"))

        valid-types-spec (s/def
                           valid-types-spec-name
                           (all-fields-correct-types?-provider entity-model))

        valid-fields-spec-name (keyword
                                 (str entity-key
                                      "-fields-valid"))

        valid-fields-spec (s/def
                            valid-fields-spec-name
                            (all-fields-valid?-provider entity-model))]

    (s/and
      has-mandatory-fields-spec
      valid-types-spec
      valid-fields-spec)))