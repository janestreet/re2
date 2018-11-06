open! Core_kernel

type t = [
  | `Encoding_latin1 of bool
  | `Posix_syntax of bool
  | `Longest_match of bool
  | `Log_errors of bool
  | `Max_mem of int
  | `Literal of bool
  | `Never_nl of bool
  | `Dot_nl of bool
  | `Never_capture of bool
  | `Case_sensitive of bool
  | `Perl_classes of bool
  | `Word_boundary of bool
  | `One_line of bool
]
[@@deriving compare, sexp_of]

(** [ `Encoding_latin1 true ] *)
val latin1 : t list

(** [ `Posix_syntax true; `Longest_match true ] *)
val posix : t list

(** [ `Log_errors true ] *)
val noisy : t list

module C_repr : sig
  type t
end

val to_c_repr : t list -> C_repr.t

val of_c_repr : C_repr.t -> t list

module Private : sig
  val examples_for_testing : t list
end
