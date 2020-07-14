(ns modeling-framework.spec
  "Spec of the data structures used for describing models"
  (:require [clojure.spec.alpha :as s :refer [coll-of]]))


(def persistence-types
  #{::keyword
    ::string
    ::long
    ::boolean
    ::instant
    ::big-int
    ::float
    ::double
    ::big-dec
    ::uuid
    ::uri
    ::bytes
    ::ref})

(def cardinality-values
  #{::single
    ::multiple})

(s/def ::i18n-ref keyword?)
(s/def ::id  keyword?)
(s/def ::label (s/or :literal string? :keyword ::i18n-ref))
(s/def ::spec keyword?)
(s/def ::description string?)
(s/def ::persistence-type #(contains? persistence-types %))
(s/def ::cardinality #(contains? cardinality-values %))
(s/def ::required boolean?)
(s/def ::identifies boolean?)



(s/def ::attribute
  (s/keys :req-un [::id ::label ::spec ::persistence-type]
          :opt-un [::description ::cardinality ::required ::identifies]))

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