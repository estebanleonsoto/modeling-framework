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

;;Candidate to removal
(defmacro register-spec!
  "Registers a spec with the given spec key and the corresponding spec body
  in the registry. This can be used to register specs whose name will be
  calculated at runtime."
  [spec-key body]
  (let [spec-key-value (eval spec-key)]
    `(s/def ~spec-key-value ~body)))

;; Candidate to removal
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

(defn attribute-ids [entity]
  (->> entity
       (:attributes)
       (map :id)
       (vec)))

(defn non-subentity-attributes [entity-model]
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
  (= (type (entity (:id attribute-model)))
     (persistence-types (:persistence-type attribute-model))))

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
                        simple-attributes (non-subentity-attributes entity-model)]
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
                                                  (set))
                        _ (println "s-ents--->  " sub-entities-entries "::: atts ---> " attributes)]
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
                        simple-attributes (non-subentity-attributes entity-model)]
                    (->> simple-attributes
                         (map (fn [attribute]
                                `(s/def
                                   ~(entity-has-attribute-with-type-correct-kw entity-id attribute)
                                   (attribute-correct-type-predicate ~(attribute-model attribute entity-model))))))))))
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
           (filter #(not (empty? %)))))))

(defn has-all-attributes-valid-kw [entity-id]
  (decorate-keyword entity-id nil "-with-attributes-valid"))

(defn entity-has-sub-entity-entry-with-attributes-valid-kw [entity-id sub-entity-entry]
  (decorate-keyword entity-id nil (str "-has-" (name sub-entity-entry) "-with-all-attributes-valid")))

(defn entity-has-attribute-valid-kw [entity-id attribute-id]
  (decorate-keyword entity-id nil (str "-has-" (name attribute-id) "-valid")))

(defn collection-elements-valid [attribute-model entity]
  (let [attribute-value (entity (:id attribute-model))
        the-spec (attribute-model :spec)]
    (or (nil? the-spec)
        (and (coll? attribute-value)
             (every?
               #(s/valid? the-spec %)
               attribute-value)))))

(defn single-element-valid [attribute-model entity]
  (let [attribute-spec (attribute-model :spec)]
    (or (nil? attribute-spec)
        (s/valid?
          attribute-spec
          (entity (:id attribute-model))))))

(defn attribute-valid-predicate [attribute-model]
  (fn [entity]
    (or (not (contains? entity (:id attribute-model)))
        (if (= (attribute-model :cardinality) ::multiple)
          (collection-elements-valid attribute-model entity)
          (single-element-valid attribute-model entity)))))

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
                           (map (fn [attribute]
                                  `(s/def ~(entity-has-attribute-valid-kw entity-id attribute) true))))))))
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
                       ~(has-all-attributes-valid-kw entity-id)
                       ~(->> attributes
                             (map (fn [attribute]
                                    (if (contains? sub-entities-entries attribute)
                                      (entity-has-sub-entity-entry-with-attributes-valid-kw entity-id attribute)
                                      (entity-has-attribute-valid-kw entity-id attribute))))
                             (cons `s/and)))))))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        simple-attributes (non-subentity-attributes entity-model)]
                    (->> simple-attributes
                         (map (fn [attribute]
                                `(s/def
                                   ~(entity-has-attribute-valid-kw entity-id attribute)
                                   (attribute-valid-predicate ~(attribute-model attribute entity-model)))))))))
           (apply concat))
      (->> entities
           (map (fn [entity-model]
                  (let [entity-id (:id entity-model)
                        sub-entities (sub-entities entity-model)]
                    (->> sub-entities
                         (map (fn [[sub-entity-entry sub-entity-id sub-entity-cardinality]]
                                (let [entity-param (gensym "entity-")]
                                  `(s/def
                                     ~(entity-has-sub-entity-entry-with-attributes-valid-kw entity-id sub-entity-entry)
                                     (fn [~entity-param]
                                       (s/valid?
                                         ~(has-all-attributes-valid-kw sub-entity-id)
                                         (~sub-entity-entry ~entity-param)))))))))))
           (filter #(not (empty? %)))
           (apply concat)))))



;; Candidate for deleting
(defmacro model-required-attributes-spec
  "Registers specs for all mandatory attribuets in the entities in the provided model"
  [model]
  (let [entity-param (gensym "entity-")]
    `(for [~entity-param (:entities ~model)]
       (entity-required-attributes-spec ~entity-param))))

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
  (let [model-value (eval model)
        entity-var (gensym "entity-var-")]
    `(do
       (register-attributes-required-specs ~model)
       (register-attributes-valid-specs ~model)
       (register-correct-types-specs ~model))))
