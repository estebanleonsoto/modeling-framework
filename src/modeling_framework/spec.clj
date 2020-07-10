(ns modeling-framework.spec
  "Spec of the data structures used for describing models"
  (:require [clojure.spec.alpha :as s :refer [coll-of]]))


(s/def ::i18n-ref keyword?)
(s/def ::id  keyword?)
(s/def ::label (s/or :literal string? :keyword ::i18n-ref))
(s/def ::spec keyword?)
(s/def ::ref ::id)
(s/def ::description string?)


(s/def ::attribute
  (s/keys :req-un [::id ::label ::type]
          :opt-un [::description ::cardinality]))

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

(defn infer-type [spec]
  (-> spec
      s/form
      type))