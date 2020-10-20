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

(s/def
  ::qualified-keyword
  (s/and keyword? qualified-ident?))

(s/def ::i18n-ref keyword?)
(s/def ::id ::qualified-keyword)
(s/def ::label (s/or :literal string? :keyword ::i18n-ref))
(s/def ::spec keyword?)
(s/def ::description string?)
(s/def ::persistence-type #(not (nil? (persistence-types %))))
(s/def ::cardinality #(contains? cardinality-values %))
(s/def ::required boolean?)
(s/def ::identifies boolean?)
(s/def ::sub-entity ::id)
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

(defn required-attributes [entity-model]
  (->> entity-model
       (:attributes)
       (filter :required)
       (map :id)
       (vec)))

(defn sub-entities [entity-model]
  (->> entity-model
       (:attributes)
       (map #(vector (:id %) (:sub-entity %) (or (:cardinality %) ::single)))
       (filter #(not (nil? (second %))))
       (vec)))

(def required-attributes-spec-name-suffix "-with-all-required-attributes")

(defn decorate-keyword [k pre post]
  "Returns a keyword decorated with given prefix and suffix (post) around the
  name part of the keyword, leaving the namespace part untouched"
  (keyword (namespace k)
           (str (or pre "")
                (name k)
                (or post ""))))

(defn has-all-required-attributes-kw [entity-id]
  (decorate-keyword entity-id nil required-attributes-spec-name-suffix))

(defn entity-has-sub-entity-entry-with-all-required-attributes-kw [entity-id sub-entity-entry]
  (decorate-keyword entity-id nil (str "-has-" (name sub-entity-entry) "-with-all-required-attributes")))


(defmacro register-attributes-required-specs
  "Registers all mandatory attributes specs related to entities in this model"
  [model]
  (let [entities (->> model
                      (eval)
                      (:entities))]
    (concat
      `(do)
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first))]
                    (concat
                      [`(s/def
                          ~(has-all-required-attributes-kw entity-id)
                          true)]
                      (->> sub-entities-entries
                           (map (fn [sub-entity-entry]
                                  `(s/def ~(entity-has-sub-entity-entry-with-all-required-attributes-kw entity-id sub-entity-entry) true))))))))
           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        required-attributes (required-attributes entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first))]
                    `(s/def
                       ~(has-all-required-attributes-kw entity-id)
                       ~(->> sub-entities-entries
                             (map (fn [sub-entity-entry]
                                    (entity-has-sub-entity-entry-with-all-required-attributes-kw entity-id sub-entity-entry)))
                             (cons `(s/keys :req ~required-attributes))
                             (cons `s/and)))))))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-param (gensym "entity-param-")
                        entity-elem (gensym "entity-element-")
                        entity-id (:id entity-model)
                        sub-entities (->> entity-model
                                          (sub-entities))]
                    (->> sub-entities
                         (map (fn [[sub-entity-entry sub-entity-id sub-entity-cardinality]]
                                `(s/def ~(entity-has-sub-entity-entry-with-all-required-attributes-kw entity-id sub-entity-entry)
                                   (fn [~entity-param]
                                     (if (nil? (get ~entity-param ~sub-entity-entry))
                                       true
                                       (if (= ~sub-entity-cardinality ::single)
                                         (s/valid?
                                           ~(has-all-required-attributes-kw sub-entity-id)
                                           (get ~entity-param ~sub-entity-entry))
                                         (every?
                                           #(s/valid?
                                              ~(has-all-required-attributes-kw sub-entity-id)
                                              %)
                                           (get ~entity-param ~sub-entity-entry))))))))))))

           (filter #(not (empty? %)))
           (apply concat)))))

(defn has-all-types-correct-kw [entity-id]
  (decorate-keyword entity-id nil "-with-all-types-correct"))

(defn entity-has-sub-entity-entry-with-all-types-correct-kw [entity-id sub-entity-entry]
  (decorate-keyword entity-id nil (str "-has-" (name sub-entity-entry) "-with-all-types-correct")))

(defn entity-has-attribute-with-type-correct-kw [entity-id attribute-id]
  (decorate-keyword entity-id nil (str "-has-" (name attribute-id) "-with-correct-type")))

(defn attribute-ids-with-spec [entity]
  (->> entity
       (:attributes)
       (filter #(not (nil? (:spec %))))
       (map :id)
       (vec)))

(defn attribute-ids [entity]
  (->> entity
       (:attributes)
       (map :id)
       (vec)))

(defn non-subentity-attributes [entity-model]
  (->> entity-model
       (:attributes)
       (filter #(nil? (:sub-entity %)))
       (vec)))

(defn non-subentity-attribute-ids [entity-model]
  (->> entity-model
       (:attributes)
       (filter #(nil? (:sub-entity %)))
       (map :id)
       (vec)))

(defn collection-elements-correct-type [attribute-model entity]
  (let [attribute-value (entity (:id attribute-model))
        element-type (get persistence-types (:persistence-type attribute-model))]
    (and (coll? attribute-value)
         (every?
           #(= (type %) element-type)
           attribute-value))))

(defn single-element-correct-type [attribute-model entity]
  (let [attribute-id (:id attribute-model)
        persistence-type (:persistence-type attribute-model)
        expected-type (persistence-types persistence-type)]
    (= (type (entity attribute-id))
       expected-type)))


(defn attribute-correct-type-predicate [attribute-model]
  (fn [entity]
    (or (not (contains? entity (:id attribute-model)))
        (if (= (attribute-model :cardinality) ::multiple)
          (collection-elements-correct-type attribute-model entity)
          (single-element-correct-type attribute-model entity)))))

(defn attribute-model [attribute-id entity-model]
  (->> entity-model
       (:attributes)
       (filter #(= attribute-id (:id %)))
       (first)))

(defmacro register-correct-types-specs
  "Registers all correct-types specs"
  [model]
  (let [entities (->> model
                      (eval)
                      (:entities))]
    (concat
      `(do)
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first))
                        simple-attributes (non-subentity-attribute-ids entity-model)]
                    (concat
                      [`(s/def
                          ~(has-all-types-correct-kw entity-id)
                          true)]
                      (->> sub-entities-entries
                           (map (fn [sub-entity-entry]
                                  `(s/def ~(entity-has-sub-entity-entry-with-all-types-correct-kw entity-id sub-entity-entry) true))))
                      (->> simple-attributes
                           (map (fn [attribute]
                                  `(s/def ~(entity-has-attribute-with-type-correct-kw entity-id attribute) true))))))))
           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        attributes (attribute-ids entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first)
                                                  (set))]
                    `(s/def
                       ~(has-all-types-correct-kw entity-id)
                       ~(->> attributes
                             (map (fn [attribute]
                                    (if (contains? sub-entities-entries attribute)
                                      (entity-has-sub-entity-entry-with-all-types-correct-kw entity-id attribute)
                                      (entity-has-attribute-with-type-correct-kw entity-id attribute))))
                             (cons `s/and)))))))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        simple-attributes (non-subentity-attribute-ids entity-model)]
                    (->> simple-attributes
                         (map (fn [attribute]
                                `(s/def
                                   ~(entity-has-attribute-with-type-correct-kw entity-id attribute)
                                   (attribute-correct-type-predicate ~(attribute-model attribute entity-model)))))))))
           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities (sub-entities entity-model)]
                    (->> sub-entities
                         (map (fn [[sub-entity-entry sub-entity-id sub-entity-cardinality]]
                                (let [entity-param (gensym "entity-")]
                                  `(s/def
                                     ~(entity-has-sub-entity-entry-with-all-types-correct-kw entity-id sub-entity-entry)
                                     (fn [~entity-param]
                                       (s/valid?
                                         ~(has-all-types-correct-kw sub-entity-id)
                                         (~sub-entity-entry ~entity-param)))))))))))
           (filter #(not (empty? %)))
           (apply concat)))))

(defn has-all-attributes-valid-kw [entity-id]
  (decorate-keyword entity-id nil "-with-attributes-valid"))

(defn entity-has-sub-entity-entry-with-attributes-valid-kw [entity-id sub-entity-entry]
  (decorate-keyword entity-id nil (str "-has-" (name sub-entity-entry) "-with-all-attributes-valid")))

(defn entity-has-attribute-valid-kw [entity-id attribute-id]
  (decorate-keyword entity-id nil (str "-has-" (name attribute-id) "-valid")))

(defmacro register-attributes-valid-specs
  "Registers all attributes valid specs"
  [model]
  (let [entities (->> model
                      (eval)
                      (:entities))]
    (concat
      `(do)
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first))
                        simple-attributes (non-subentity-attributes entity-model)]
                    (concat
                      [`(s/def
                          ~(has-all-attributes-valid-kw entity-id)
                          true)]
                      (->> sub-entities-entries
                           (map (fn [sub-entity-entry]
                                  `(s/def ~(entity-has-sub-entity-entry-with-attributes-valid-kw entity-id sub-entity-entry) true))))
                      (->> simple-attributes
                           (filter #(not (nil? (:spec %))))
                           (map :id)
                           (map (fn [attribute-id]
                                  `(s/def ~(entity-has-attribute-valid-kw entity-id attribute-id) true))))))))
           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        simple-attribute-ids (attribute-ids-with-spec entity-model)
                        sub-entities-entries (->> entity-model
                                                  (sub-entities)
                                                  (map first)
                                                  (set))]
                    `(s/def
                       ~(has-all-attributes-valid-kw entity-id)
                       ~(cons `s/and
                              (concat
                                (->> simple-attribute-ids
                                     (map (fn [attribute-id]
                                            (entity-has-attribute-valid-kw entity-id attribute-id))))
                                (->> sub-entities-entries
                                     (map (fn [sub-entity-attribute-id]
                                            (entity-has-sub-entity-entry-with-attributes-valid-kw entity-id sub-entity-attribute-id)))))))))))

      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        simple-attributes (non-subentity-attributes entity-model)]
                    (->> simple-attributes
                         (filter #(not (nil? (:spec %))))
                         (map (fn [attribute-model]
                                (let [attribute-id (:id attribute-model)
                                      attribute-spec (:spec attribute-model)
                                      multiple? (= ::multiple (:cardinality attribute-model))
                                      entity-param (gensym "entity-")]
                                  `(s/def
                                     ~(entity-has-attribute-valid-kw entity-id (:id attribute-model))
                                     (fn [~entity-param]
                                       (s/valid?
                                         ~(if multiple?
                                            `(s/every ~attribute-spec)
                                            attribute-spec)
                                         (~attribute-id ~entity-param)))))))))))

           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities (sub-entities entity-model)]
                    (->> sub-entities
                         (map (fn [[sub-entity-entry sub-entity-id sub-entity-cardinality]]
                                (let [entity-param (gensym "entity-")
                                      multiple? (= sub-entity-cardinality ::multiple)]
                                  `(s/def
                                     ~(entity-has-sub-entity-entry-with-attributes-valid-kw entity-id sub-entity-entry)
                                     (fn [~entity-param]
                                       (s/valid?
                                         ~(if multiple?
                                            `(s/every ~(has-all-attributes-valid-kw sub-entity-id))
                                            (has-all-attributes-valid-kw sub-entity-id))
                                         (~sub-entity-entry ~entity-param)))))))))))
           (filter #(not (empty? %)))
           (apply concat)))))

(defmacro load-specs-for-model [model]
  (let [model-value (eval model)
        entity-var (gensym "entity-var-")]
    `(do
       (register-attributes-required-specs ~model)
       (register-attributes-valid-specs ~model)
       (register-correct-types-specs ~model))))
