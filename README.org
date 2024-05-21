* flatBuffers

*Note*: You probably want to use https://github.com/treeform/flatty,
unless you want a library that works on a ~Buffer~ type using manual
allocation instead of stdlib ~string~.

This is a helper package to make it easier to copy (potentially
nested) Nim objects via procedures that only work on flat datatypes.

Typically, whenever Nim objects are used that either contain ~string~
or ~seq~ types, anything involving ~copyMem~ or otherwise procedures
that require the data to be flat in memory are out.

By using ~asFlat~ (possibly combined with ~toString~) you can
convert a Nim object into a ~Buffer~ (or ~string~). Using ~flatTo[T]~ you convert
a ~Buffer~ back to a ~T~. ~fromString~ can be used to reconstruct a
~Buffer~ from a ~string~ created via ~toString~.

** Why yet another flat binary library?

This is based on a bespoke library used in [[https://github.com/Vindaar/nimhdf5][nimhdf5]] that deals with
strings and seqs (any variable length data) in a special way related
to how HDF5 stores its data. When I needed the functionality for
~flatBuffers~ I just quickly turned that nimhdf5 submodule into this
library. At the time I had forgotten [[https://github.com/treeform/flatty][flatty]] is even at thing.
