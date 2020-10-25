# modeling-framework-internal

A Clojure library designed develop business models and automatically produce source code or tools
that work on the specified model, like database interactions and luminus features.

## Usage

First you define a business model like the one in the following example:

```
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
               :sub-entity       ::address}
              {:id               ::category
               :spec             ::category-spec
               :label            "Category"
               :description      "The category the client belongs to"
               :cardinality      ::m/multiple
               :persistence-type ::m/keyword}]}
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
```
You can use the function load-specs-for-model online-shop-model in modeling-framework.spec to register 
specs based on the model. These specs can be used to evaluate the correctness of values following
the specification from the model (types, required types and custom specs from the model attributes)
```
(m/load-specs-for-model online-shop-model) ; m is the alias for modeling-framework.spec
```
Look for specs starting with the entity names in the model (here ```::client...``` for example)

You can also create schema data for datomic databases using the functions modeling-framework.datomic/schema for 
schemas to use directly with the database.
For users working in a luminus project, the conformity-based schema for the migration edn file can be generated using 
the function modeling-framework.datomic/conformity-transaction-data.



## License

Copyright © 2020 Esteban León

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
