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

(defn spec-keyword [model-id suffix]
  (keyword
    (str (namespace model-id))
    (str (name model-id) suffix)))

(defn- sub-entities [entity-model]
  (->> entity-model
       (:attributes)
       (map #(vector (:id %) (:sub-entity %)))
       (filter #(not (nil? (second %))))
       (vec)))

(defn- get-id-or-complain [model]
  (let [id (:id model)]
    (when (or (not (map? model)) (nil? id))
      (throw (new IllegalArgumentException (str "Model is missing mandatory :id field. Model: '" model "'"))))
    id))

(defn has-sub-with-all-required-atts-keyword [container-entity-id sub-entity-id]
  (keyword (namespace container-entity-id)
           (str (name container-entity-id)
                "-has-"
                (name sub-entity-id)
                "-with-all-required-attributes")))

(def required-attributes-spec-name-suffix "-with-all-required-attributes")

(defmacro mandatory-attributes-entity-spec [entity-model-symbol]
  (let [entity-model (eval entity-model-symbol)
        sub-entities (sub-entities entity-model)
        entity-id (get-id-or-complain entity-model)
        spec-id (spec-keyword entity-id required-attributes-spec-name-suffix)]

    `(do
       ~(->> sub-entities
             (map (fn [[field-name sub-entity-id]]
                    (let [entity-param (gensym "father-entity-param-")
                          sub-entity-value-param (gensym "sub-entity-value-param-")]
                      `(s/def
                         ~(has-sub-with-all-required-atts-keyword entity-id sub-entity-id)
                         (fn [~entity-param]
                           (let [~sub-entity-value-param (~field-name ~entity-param)]
                             (if (nil? ~sub-entity-value-param)
                               true
                               (s/valid? ~(spec-keyword sub-entity-id required-attributes-spec-name-suffix)
                                         ~sub-entity-value-param))))))))
             (cons `do))
       ~(if (empty? sub-entities)
          `(s/def ~spec-id (s/keys :req ~(required-attributes entity-model)))
          `(s/def ~spec-id
             ~(->> sub-entities
                   (map second)
                   (map #(has-sub-with-all-required-atts-keyword entity-id %))
                   (cons `(s/keys :req ~(required-attributes entity-model)))
                   (cons `s/and)))))))

(defn decorate-keyword [k pre post]
  "Returns a keyword decorated with given prefix and suffix (post) around the
  name part of the keyword, leaving the namespace part untouched"
  (keyword (namespace k)
           (str (or pre "")
                (name k)
                (or post ""))))

(defmacro register-spec!
  "Registers a spec with the given spec key and the corresponding spec body
  in the registry. This can be used to register specs whose name will be
  calculated at runtime."
  [spec-key body]
  (let [spec-key-value (eval spec-key)]
    `(s/def ~spec-key-value ~body)))

(defmacro entity-required-attributes-spec [entity]
  (let [sub-entities-param (gensym "sub-entities-")
        ;sub-entity-param (gensym "sub-entity-")
        sub-entities (sub-entities (eval entity))
        required-attributes-param (gensym "required-attributes-")
        all-sub-entities-kw-param (gensym "sub-entities-kws-")
        required-attributes-spec-name-param (gensym "required-atts-")]
    `(let [~all-sub-entities-kw-param (has-sub-with-all-required-atts-keyword (:id ~entity) ::sub-ent)
           ~required-attributes-param (required-attributes ~entity)]
       (register-spec!
         (spec-keyword (:id ~entity) required-attributes-spec-name-suffix)
         (s/and
           (s/keys :req (vector ~required-attributes-param))
           ~(cons 's/and
                  (for [sub-entity sub-entities]
                    (first sub-entity))))))))


(defn has-all-required-attributes-kw [entity-id]
  (decorate-keyword entity-id nil required-attributes-spec-name-suffix))

(defn entity-has-sub-entity-entry-with-all-required-attributes-kw [entity-id sub-entity-entry]
  (decorate-keyword entity-id nil (str "-has-" (name sub-entity-entry) "-with-all-required-attributes")))


(defmacro mandatory-attributes-model-spec
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
                        entity-id (:id entity-model)
                        sub-entities (->> entity-model
                                          (sub-entities))]
                    (->> sub-entities
                         (map (fn [[sub-entity-entry sub-entity-id]]
                                `(s/def ~(entity-has-sub-entity-entry-with-all-required-attributes-kw entity-id sub-entity-entry)
                                   (fn [~entity-param]
                                     (if (nil? (get ~entity-param ~sub-entity-entry))
                                       true
                                       (s/valid?
                                         ~(has-all-required-attributes-kw sub-entity-id)
                                         (get ~entity-param ~sub-entity-entry)))))))))))
           (filter #(not (empty? %)))))))





(defmacro model-required-attributes-spec
  "Registers specs for all mandatory attribuets in the entities in the provided model"
  [model]
  (let [entity-param (gensym "entity-")]
    `(for [~entity-param (:entities ~model)]
       (entity-required-attributes-spec ~entity-param))))

(defn single-element-correct-type [attribute-model entity]
  (= (type (entity (:id attribute-model)))
     (persistence-types (:persistence-type attribute-model))))

(defn collection-elements-correct-type [attribute-model entity]
  (let [attribute-value (entity (:id attribute-model))
        element-type (get persistence-types (:persistence-type attribute-model))]
    (and (coll? attribute-value)
         (every?
           #(= (type %) element-type)
           attribute-value))))


(defn attribute-correct-type-predicate [attribute-model]
  (fn [entity]
    (or (not (contains? entity (:id attribute-model)))
        (if (= (attribute-model :cardinality) ::multiple)
          (collection-elements-correct-type attribute-model entity)
          (single-element-correct-type attribute-model entity)))))


(defmacro correct-attribute-type-spec [attribute-model]
  `(list 's/def
         (spec-keyword (:id ~attribute-model) "-with-all-correct-types")
         (attribute-correct-type-predicate ~attribute-model)))

(defmacro join-specs [spec-ids]
  `(conj ~spec-ids 's/and))

(defmacro def-entity-correct-types-spec [entity-model attribute-types-spec]
  `(list 's/def
         (spec-keyword (:id ~entity-model) "-with-all-correct-types")
         ~attribute-types-spec))


(defn valid-value-types-spec [entity-model]
  (let [attribute-type-specs (->> entity-model
                                  (:attributes)
                                  (map #(correct-attribute-type-spec %))
                                  (map eval)
                                  (join-specs))
        all-attributes-with-correct-type (def-entity-correct-types-spec entity-model attribute-type-specs)]
    (eval all-attributes-with-correct-type)))

(defmacro def-entity-valid?-spec [entity-model attributes-valid?]
  `(list 's/def
         (spec-keyword (:id ~entity-model) "-all-attributes-valid?")
         ~attributes-valid?))

(defn all-attributes-valid-spec [entity-model]
  (let [attribute->spec (->> entity-model
                             (:attributes)
                             (filter #(:spec %))
                             (map #(vector (:id %) (:spec %)))
                             (into {}))
        attributes-valid? (fn [entity]
                            (every? #(s/valid? (attribute->spec (first %)) (second %)) entity))]
    (eval (def-entity-valid?-spec entity-model attributes-valid?))))

(defmacro load-specs-for-model [model]
  (let [model-value (eval model)]
    `(do
       (mandatory-attributes-model-spec ~model))))