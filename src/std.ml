(** The recommended way to use this library:
        [open Re2.Std]
    or
        [module Re2 = Re2.Std.Re2]
    if you only want that module.

    The older [module Regex = Re2.Regex] will remain available for a while until people
    switch over, but it is deprecated and a future feature will remove it.
 **)

module Parser = Parser
module Re2 = Re2_internal

(* Required for jane-script auto-loading *)
let () = Re2_c.linkme
