
ixset-typed
===========

This Haskell package provides a data structure of sets that are indexed
by potentially multiple indices.

Sets can be created, modified, and queried in various ways.

The package is a variant of the [ixset][1] package. The ixset package makes use
of run-time type information to find a suitable index on a query, resulting
in possible run-time errors when no suitable index exists. In ixset-typed,
the types of all indices available or tracked in the type system.
Thus, ixset-typed should be safer to use than ixset, but in turn requires
more GHC extensions.

At the moment, the two packages are relatively compatible. As a consequence
of the more precise types, a few manual tweaks are necessary when switching
from one to the other, but the interface is mostly the same.

  [1]: https://hackage.haskell.org/package/ixset
