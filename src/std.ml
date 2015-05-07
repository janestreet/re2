(** The recommended way to use this library:
        open Re2.Std
    or
        module Re2 = Re2.Std.Re2
    These are equivalent but the second is explicit about exposing only one name.

    The older "module Regex = Re2.Regex" will remain available for a while until people
    switch over, but it is deprecated and a future feature will remove it.
 **)

module Re2 = Re2_internal

