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
(s/def ::sub-entity boolean?)
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

(s/def ::model
  (s/keys :req-un [::id
                   ::entities]
          :opt-un [::description]))

(defn entity-model [model entity-key]
  (->> model
       (:entities)
       (filter #(= entity-key (:id %)))
       (first)))