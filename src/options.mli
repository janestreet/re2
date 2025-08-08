open! Core

(** See [re2_c/libre2/re2/re2.h] for documentation of these options. *)

module Encoding : sig
  type t =
    | Latin1
    | Utf8
  [@@deriving compare ~localize, sexp_of]
end

type t =
  { case_sensitive : bool
  ; dot_nl : bool
  ; encoding : Encoding.t
  ; literal : bool
  ; log_errors : bool
  ; longest_match : bool
  ; max_mem : int
  ; never_capture : bool
  ; never_nl : bool
  ; one_line : bool
  ; perl_classes : bool
  ; posix_syntax : bool
  ; word_boundary : bool
  }
[@@deriving compare ~localize, sexp_of]

val default : t

(** [latin1 = { default with encoding = Latin1 }] *)
val latin1 : t

(** [noisy = { default with log_errors = true }] *)
val noisy : t

(** [posix = { default with longest_match = true; posix_syntax = true }] *)
val posix : t

module Private : sig
  module C_repr : sig
    type t
  end

  val to_c_repr : t -> C_repr.t
  val of_c_repr : C_repr.t -> t
end

module Stable : sig
  module Encoding : sig
    module V1 : sig
      type t = Encoding.t [@@deriving hash]

      include%template
        Stable_without_comparator_with_witness [@mode local] with type t := t
    end
  end

  module V2 : sig
    type nonrec t = t [@@deriving hash]

    val is_default : t -> bool

    include%template Stable_without_comparator_with_witness [@mode local] with type t := t
  end
end
