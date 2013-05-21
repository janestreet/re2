How to link against these bindings
==================================

We export a library Re2 with one module Regex which binds the Google re2 regex
library.  Binaries which link to the OCaml Re2 library get the underlying
Google library and these bindings.

At the moment, OMake does not detect changes made to the C source during
a build, because it calls Make to build the underlying library and the C-side
stub code.  If you call OMake without -P, everything should work.  If you call
OMake with -P, you will link against C object files that are at least as
current as the C source was when OMake was first invoked.  If you call OMake
with -P and then modify the C source, you will need to restart OMake in order
to get those modifications.
