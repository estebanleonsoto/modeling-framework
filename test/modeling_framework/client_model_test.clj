(ns modeling-framework.client-model-test
  (:require [clojure.test :refer :all]
            [modeling-framework.spec :as m]
            [clojure.spec.alpha :as s]))

(s/def ::simple-string string?)
;(s/def ::category #(contains?
;                     #{:normal
;                       :whole-seller
;                       :compita} %))
(s/def ::telephone-number-spec pos-int?)
(def contact-channel-types
  #{:home
    :mobile
    :work
    :fax})
(s/def ::contact-channel-type-spec
  (s/and keyword?
         #(contains? contact-channel-types %)))
(s/def ::simple-float float?)

(def email-regex #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")
(s/def ::e-mail-spec #(re-matches email-regex %))


(def online-shop-model
  {:id ::data-model
   :entities
       [{:id ::client
         :attributes
             [{:id               ::first-name
               :spec             ::simple-string
               :label            "First-name"
               :description      "The first name of the client"
               :persistence-type ::m/string
               :required         true}
              {:id               ::last-name
               :spec             ::simple-string
               :label            "Last-name"
               :cardinality      ::m/multiple
               :description      "The last names of the client"
               :persistence-type ::m/string}
              {:id               ::telephones
               ;TODO              :spec             ::ref
               :label            "Telephone"
               :cardinality      ::m/multiple
               :description      "The type of this telephone number"
               :persistence-type ::m/ref
               :sub-entity       ::telephone}
              {:id               ::emails
               ;:spec             ::ref
               :label            :email/emails
               :cardinality      ::m/multiple
               :description      "Email addresses of the client"
               :persistence-type ::m/ref
               :sub-entity       ::email}
              {:id               ::identification
               :spec             ::simple-string
               :label            "Id"
               :description      "The reference of the client's identification"
               :cardinality      ::m/single
               :persistence-type ::m/string
               :identifies       true
               :required         true}
              {:id               ::address
               ;:spec             ::ref
               :label            "Address"
               :description      "A reference to multiple addresses in an entity"
               :cardinality      ::m/multiple
               :persistence-type ::m/ref
               :sub-entity       ::address}]}
        ;{:id               ::category
        ; :spec             ::category
        ; :label            "Category"
        ; :description      "The category the client belongs to"
        ; :cardinality      ::m/multiple
        ; :persistence-type ::m/keyword}]}
        {:id ::telephone
         :attributes
             [{:id               ::telephone-number
               :persistence-type ::m/long
               :spec             ::telephone-number-spec
               :label            :telephone/number
               :description      "The telephone number"
               :required         true}
              {:id               ::telephone-type
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :telephone/type
               :description      "The type of telephone (mobile, home, work, etc."
               :required         true}]}
        {:id ::email
         :attributes
             [{:id               ::email-address
               :persistence-type ::m/string
               :spec             ::e-mail-spec
               :label            :email/email
               :description      "The email of the client"
               :required         true}
              {:id               ::email-type
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :email/type
               :description      "The type of email (mobile, home, work, etc."
               :required         false}]}
        {:id ::address
         :attributes
             [{:id               ::address-descriptor
               :persistence-type ::m/keyword
               :spec             ::contact-channel-type-spec
               :label            :address/type
               :description      "Describes the type of address this is. For example it can be house address office address delivery address billing address etc."
               :required         true}
              {:id               ::address-lines
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/line
               :cardinality      ::m/multiple
               :description      "Street and house number details of the address"}
              {:id               ::city
               :persistence-type ::m/string
               :label            :address/city
               :spec             ::simple-string
               :description      "The city or district of the address"}
              {:id               ::canton
               :label            :address/canton
               :spec             ::simple-string
               :persistence-type ::m/string
               :description      "The canton (or county) of the province"}
              {:id               ::state-or-province
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/state-or-province
               :description      "The state or province of the address"}
              {:id               ::postal-code
               :label            :address/postal-code
               :spec             ::simple-string
               :persistence-type ::m/string
               :description      "The postal code of the address"}
              {:id               ::country
               :persistence-type ::m/string
               :spec             ::simple-string
               :label            :address/country
               :description      "The country of the address"
               :required         true}
              {:id               ::coordinates
               :persistence-type ::m/ref
               ;:spec             ::re
               :label            :address/coordinates
               :description      "The global coordinates of the address in a map"
               :sub-entity       ::coordinates}]}
        {:id ::coordinates
         :attributes
             [{:id               ::latitude
               :persistence-type ::m/float
               :label            :address/latitude
               :spec             ::simple-float
               :description      "The latitude of the location in the map in decimal degrees"
               :required         true}
              {:id               ::longitude
               :persistence-type ::m/float
               :label            :address/longitude
               :spec             ::simple-float
               :description      "The longitude of the location in the map in decimal degrees"
               :required         true}]}]})

(deftest evaluate-spec
  (testing
    "The client model must pass the spec"
    (let [good-client {::first-name     "Esteban"
                       ::last-name      ["León" "Soto"]
                       ::telephones     [{::telephone-number 123784902318794321
                                          ::telephone-type   :home}]
                       ::emails         [{::email-address "esteban@email.com"
                                          ::email-type    :home}]
                       ::identification "1-456A-5456456"
                       ::address        [{::address-descriptor :home
                                          ::address-lines      ["De la farmacia La Paulina"
                                                                "(que ya no existe)450 varas"
                                                                "al este"]
                                          ::city               "Paso de la Vaca"
                                          ::canton             "San José"
                                          ::state-or-province  "San José"
                                          ::postal-code        "10101"
                                          ::country            "Costa Rica"
                                          ::coordinates        {::latitude  9.945511223885042
                                                                ::longitude -84.12367698123887}}]
                       ::category       [:compita]}
          missing-last-name {::first-name "John"}
          missing-telephone-type {::first-name     "Esteban"
                                  ::last-name      ["León" "Soto"]
                                  ::telephones     [{::telephone-number 123784902318794321}]

                                  ::emails         [{::email-address "esteban@email.com"
                                                     ::email-type    :home}]
                                  ::identification "1-456A-5456456"
                                  ::address        [{::address-descriptor :home
                                                     ::address-lines      ["De la farmacia La Paulina"
                                                                           "(que ya no existe)450 varas"
                                                                           "al este"]
                                                     ::city               "Paso de la Vaca"
                                                     ::canton             "San José"
                                                     ::state-or-province  "San José"
                                                     ::postal-code        "10101"
                                                     ::country            "Costa Rica"
                                                     ::coordinates        {::latitude  9.945511223885042
                                                                           ::longitude -84.12367698123887}}]
                                  ::category       [:compita]}]

      (m/load-specs-for-model online-shop-model)
      (is (s/valid? ::client-with-all-required-attributes good-client))
      (is (not (s/valid? ::client-with-all-required-attributes missing-last-name)))
      (is (not (s/valid? ::client-with-all-required-attributes missing-telephone-type)))
      (is (s/valid? ::client-with-all-types-correct good-client))
      (is (s/valid? ::client-with-attributes-valid good-client)))))