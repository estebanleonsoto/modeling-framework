(ns modeling-framework.spec
  "Spec of the data structures used for describing models"
  (:require [clojure.spec.alpha :as s :refer [coll-of]]
            [clojure.pprint :refer [pprint]])
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

(defn fetch-entity-model [model entity-key]
  (->> model
       (:entities)
       (filter #(= entity-key (:id %)))
       (first)))

(defn mandatory-attributes [entity-model]
  (->> entity-model
       (:attributes)
       (filter :required)
       (map :id)
       (vec)))

(defmacro mandatory-attributes-spec [entity-model]
  (let [entity-id `(:id ~entity-model)
        spec-id `(keyword
                   (namespace ~entity-id)
                   (str (name ~entity-id) "-with-all-required-attributes"))]
    `(list 's/def
           ~spec-id
           (list
             's/keys
             ':req
             (mandatory-attributes ~entity-model)))))

(defn missing-mandatory-attribute [entity-model entity]
  (let [mandatory-fields (->> entity-model
                              (:attributes)
                              (filter :required)
                              (map :id))]
    (->> mandatory-fields)))


(defn correct-type? [field-type value]
  (if (= (field-type 0) (type []))
    (= (type (value 0)) (field-type 1))
    (= (type value) (field-type 0))))


(defn all-fields-correct-types?-provider [entity-model]
  (let [field-to-type (->> entity-model
                           (:attributes)
                           (map #(vector (:id %) (if (= ::multiple (:cardinality %))
                                                   (vector (type []) (persistence-types (:persistence-type %)))
                                                   (vector (persistence-types (:persistence-type %))))))

                           (into {}))]
    (fn [entity]
      (->> entity
           (map #(correct-type? (field-to-type (first %)) (last %)))
           (every? true?)))))

(def test-model
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
               :cardinality      ::multiple
               :description      "The last names of the client"
               :persistence-type ::string
               :required         true}
              {:id               ::identification
               :spec             ::simple-string
               :label            "Identification"
               :description      "The identification of the client"
               :persistence-type ::string
               :required         true}]}]})

(def test-client
  {::first-name     "Esteban"
   ::last-name      "Lionzo"
   ::identification "12345453421"})

(def test-client-no-ns
  {:first-name     "Esteban"
   :last-name      "Lionzo"
   :identification "12345453421"})


(def test-client-bad
  {::first-name "Esteban"})
