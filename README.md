# pure-clj (Purescript Clojure backend)

A small strongly typed programming language with expressive types that compiles to ~~JavaScript~~ Clojure, written in and inspired by Haskell.

This is not a fork of the compiler, but a different compiler that translates purescript corefn output to Clojure.

The [pure-clj](https://github.com/pure-clj) organisation hosts ports of some core libraries.

To use it, it is recommended to use psc-package and the pure-clj package sets.

## Usage

Install [purescript](https://github.com/purescript/purescript) and have `purs` on your path.

Install pure-clj from this repository, compile it and put it on your path (as `pursclj`).

Install [psc-package](https://github.com/purescript/psc-package).

Install [leiningen](https://leiningen.org/) then:

```
lein new pure-clj `project-name`
```

This will create a new pure-clj project. Change the `set` in the psc-package.json file to the latest clj set in the [repo](https://github.com/pure-clj/package-sets).

Now you can use `lein pursclj` to compile your Purescript project to Clojure and `lein testp` to run tests (configurable in the project.clj).

## Types

| Purescript type | Clojure/Java type | Notes |
| --- | --- | --- |
| `Int` | `long` | 64 bits Java long |
| `Boolean` | `boolean` | |
| `String` | `String` | |
| `Array` | `clojure.lang.PersistentVector` | Compiles to Clojure's persistent vectors |
| Records | `clojure.lang.PersistentArrayMap` | Compiles to Clojure's persistent maps |
| Tagged Union | Clojure `defrecord` | |
| Newtype | as underlying type | |
| Functions | Clojure functions curried | `foo a b = ...` -> `(def foo (fn [a] (fn [b] ...)))` |

## FFI

Instead of `.js` files, foreign files should be valid Clojure (`.clj`) files and should be placed together with the original `.purs` file.