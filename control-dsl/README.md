# `Control.Dsl`: An alternative to monads

The `Prelude.>>=` combinator limits the return type as a `m b`, which cause Monads not composable. In order to ease this restrictions, this proposal introduces another type class `Dsl` for do notation.

Motivation
----------

Monads do not compose. Normally a `do` block cannot contains operators defined in different monad instances.

The state of art solution is [using only one monad](http://okmij.org/ftp/Haskell/extensible/) `Eff`, which forwards all monadic bind operations to custom effect handlers, instead of defining new monads.

However, the `Eff` approach is heavy weight than ordinary monad. It's not very convenient to create an additional indirect layer for simple use cases.

The  `Eff` approach of bypassing  `>>=` combinator is quite embarrassing. Since  `>>=` settles on our logo, In this proposal we present a new approach to enable multiple operations at once by improving  `>>=`. This proposal aims to port the approach used in [Dsl.scala](https://github.com/ThoughtWorksInc/Dsl.scala) to Haskell. This approach improves the extensibility of `>>=`.
