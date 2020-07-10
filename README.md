# modeling-framework
[![Build Status](https://travis-ci.org//.svg?branch=master)](https://travis-ci.org//)
[![codecov](https://codecov.io/gh///branch/master/graph/badge.svg)](https://codecov.io/gh//)
[![Clojars Project](https://img.shields.io/clojars/v/modeling-framework.svg)](https://clojars.org/modeling-framework)

A Clojure library designed to provide a way to describe your domain models.

Along that, this library is planned to provide features that allow exporting the model to different scenarios, producing implementation 
of it in areas like database schemas, APIs, front-end forms, etc.

```clj
[modeling-framework "0.0.0"]
```

## Usage

Define a model using :edn such that it conforms to the the spec in this library.
This model can be used as input in the various functions that export it to other scenarios.

## License

Copyright © 2020

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
