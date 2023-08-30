module Stable0 = struct
  open! Core.Core_stable

  module Encoding = struct
    module V1 = struct
      type t =
        | Latin1
        | Utf8
      [@@deriving bin_io, compare, hash, sexp, stable_witness]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bf67b13f243e7b82146959041854651d |}]
      ;;
    end
  end

  (* This [Serialization.t] is the serialization of [t] and it's slightly
     different from [t]:
     - Serialization.t has [case_insensitive] instead of [case_sensitive], since
       Re2.Options.default has [case_sensitive] as the only field that is [true].
       By using [case_insensitive] we have the nice property where default representation
       has all bool as false (and an empty sexp).
     - [max_mem] is stripped (and populated with default) during
       serialisation, since I don't think it makes sense to serialise this
     - it seems that some parameters in Re2.Options.t have implied values,
       so there might be room for improvement of this [t] (at the cost of
       more complex code here); for example posix_syntax=false implies that
       some of the other parameters are actually ignored
  *)
  module V2 = struct
    module Serialization = struct
      type t =
        { case_insensitive : bool [@sexp.bool]
        ; dot_nl : bool [@sexp.bool]
        ; encoding : Encoding.V1.t
             [@sexp.default Encoding.V1.Utf8] [@sexp_drop_default.compare]
        ; literal : bool [@sexp.bool]
        ; log_errors : bool [@sexp.bool]
        ; longest_match : bool [@sexp.bool]
        ; never_capture : bool [@sexp.bool]
        ; never_nl : bool [@sexp.bool]
        ; one_line : bool [@sexp.bool]
        ; perl_classes : bool [@sexp.bool]
        ; posix_syntax : bool [@sexp.bool]
        ; word_boundary : bool [@sexp.bool]
        }
      [@@deriving bin_io, compare, hash, sexp, stable_witness]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7e4458318a614214b63cb4b98577c10a |}]
      ;;
    end
  end
end

open! Core

module Encoding = struct
  type t = Stable0.Encoding.V1.t =
    | Latin1
    | Utf8
  [@@deriving compare, equal, sexp_of]

  module C_repr = struct
    type t = int [@@deriving compare, sexp_of]

    let equal = Int.( = )

    (* would use [@@deriving equal], but equal_int is not in scope *)

    external get_latin1 : unit -> int = "mlre2__options__encoding__get_latin1" [@@noalloc]
    external get_utf8 : unit -> int = "mlre2__options__encoding__get_utf8" [@@noalloc]

    let utf8 = get_utf8 ()
    let latin1 = get_latin1 ()
  end

  let to_c_repr = function
    | Latin1 -> C_repr.latin1
    | Utf8 -> C_repr.utf8
  ;;

  let of_c_repr c_repr =
    if C_repr.equal c_repr C_repr.utf8
    then Utf8
    else if C_repr.equal c_repr C_repr.latin1
    then Latin1
    else raise_s [%message "Unexpected Encoding.C_repr" ~_:(c_repr : C_repr.t)]
  ;;
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
[@@deriving
  compare, fields ~getters ~iterators:make_creator ~direct_iterators:iter, sexp_of]

module C_repr = struct
  type t

  (*$ Re2_options_cinaps.print_c_repr_external_bindings () *)
  external case_sensitive : t -> bool = "mlre2__options__case_sensitive" [@@noalloc]

  external set_case_sensitive : t -> bool -> unit = "mlre2__options__set_case_sensitive"
    [@@noalloc]

  external dot_nl : t -> bool = "mlre2__options__dot_nl" [@@noalloc]
  external set_dot_nl : t -> bool -> unit = "mlre2__options__set_dot_nl" [@@noalloc]
  external encoding : t -> Encoding.C_repr.t = "mlre2__options__encoding" [@@noalloc]

  external set_encoding : t -> Encoding.C_repr.t -> unit = "mlre2__options__set_encoding"
    [@@noalloc]

  external literal : t -> bool = "mlre2__options__literal" [@@noalloc]
  external set_literal : t -> bool -> unit = "mlre2__options__set_literal" [@@noalloc]
  external log_errors : t -> bool = "mlre2__options__log_errors" [@@noalloc]

  external set_log_errors : t -> bool -> unit = "mlre2__options__set_log_errors"
    [@@noalloc]

  external longest_match : t -> bool = "mlre2__options__longest_match" [@@noalloc]

  external set_longest_match : t -> bool -> unit = "mlre2__options__set_longest_match"
    [@@noalloc]

  external max_mem : t -> int = "mlre2__options__max_mem" [@@noalloc]
  external set_max_mem : t -> int -> unit = "mlre2__options__set_max_mem" [@@noalloc]
  external never_capture : t -> bool = "mlre2__options__never_capture" [@@noalloc]

  external set_never_capture : t -> bool -> unit = "mlre2__options__set_never_capture"
    [@@noalloc]

  external never_nl : t -> bool = "mlre2__options__never_nl" [@@noalloc]
  external set_never_nl : t -> bool -> unit = "mlre2__options__set_never_nl" [@@noalloc]
  external one_line : t -> bool = "mlre2__options__one_line" [@@noalloc]
  external set_one_line : t -> bool -> unit = "mlre2__options__set_one_line" [@@noalloc]
  external perl_classes : t -> bool = "mlre2__options__perl_classes" [@@noalloc]

  external set_perl_classes : t -> bool -> unit = "mlre2__options__set_perl_classes"
    [@@noalloc]

  external posix_syntax : t -> bool = "mlre2__options__posix_syntax" [@@noalloc]

  external set_posix_syntax : t -> bool -> unit = "mlre2__options__set_posix_syntax"
    [@@noalloc]

  external word_boundary : t -> bool = "mlre2__options__word_boundary" [@@noalloc]

  external set_word_boundary : t -> bool -> unit = "mlre2__options__set_word_boundary"
    [@@noalloc]
  (*$*)

  external create_quiet : unit -> t = "mlre2__options__create_quiet"
end

let to_c_repr t =
  let c_repr = C_repr.create_quiet () in
  let f set _field _t value = set c_repr value in
  Fields.Direct.iter
    t (*$ Re2_options_cinaps.print_to_c_repr_fields () *)
    ~case_sensitive:(f C_repr.set_case_sensitive)
    ~dot_nl:(f C_repr.set_dot_nl)
    ~encoding:
      (f (fun c_repr value -> C_repr.set_encoding c_repr (Encoding.to_c_repr value)))
    ~literal:(f C_repr.set_literal)
    ~log_errors:(f C_repr.set_log_errors)
    ~longest_match:(f C_repr.set_longest_match)
    ~max_mem:(f C_repr.set_max_mem)
    ~never_capture:(f C_repr.set_never_capture)
    ~never_nl:(f C_repr.set_never_nl)
    ~one_line:(f C_repr.set_one_line)
    ~perl_classes:(f C_repr.set_perl_classes)
    ~posix_syntax:(f C_repr.set_posix_syntax)
    ~word_boundary:(f C_repr.set_word_boundary)
  (*$*);
  c_repr
;;

let of_c_repr =
  let f get _field () = get, () in
  Fields.make_creator (*$ Re2_options_cinaps.print_of_c_repr_fields () *)
    ~case_sensitive:(f C_repr.case_sensitive)
    ~dot_nl:(f C_repr.dot_nl)
    ~encoding:(f (fun c_repr -> Encoding.of_c_repr (C_repr.encoding c_repr)))
    ~literal:(f C_repr.literal)
    ~log_errors:(f C_repr.log_errors)
    ~longest_match:(f C_repr.longest_match)
    ~max_mem:(f C_repr.max_mem)
    ~never_capture:(f C_repr.never_capture)
    ~never_nl:(f C_repr.never_nl)
    ~one_line:(f C_repr.one_line)
    ~perl_classes:(f C_repr.perl_classes)
    ~posix_syntax:(f C_repr.posix_syntax)
    ~word_boundary:(f C_repr.word_boundary) (*$*)
    ()
  |> fst
;;

let default = C_repr.create_quiet () |> of_c_repr
let latin1 = { default with encoding = Latin1 }
let noisy = { default with log_errors = true }
let posix = { default with longest_match = true; posix_syntax = true }
let default_max_mem = max_mem default

module Private = struct
  module C_repr = C_repr

  let of_c_repr = of_c_repr
  let to_c_repr = to_c_repr
end

module Stable = struct
  open! Stable_witness.Export
  include Stable0

  module V2 = struct
    module Serialization = V2.Serialization

    type nonrec t = t =
      { case_sensitive : bool
      ; dot_nl : bool
      ; encoding : Encoding.V1.t
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
    [@@deriving compare, hash, stable_witness]

    let to_serialization
      { case_sensitive
      ; dot_nl
      ; encoding
      ; literal
      ; log_errors
      ; longest_match
      ; max_mem = _
      ; never_capture
      ; never_nl
      ; one_line
      ; perl_classes
      ; posix_syntax
      ; word_boundary
      }
      : Serialization.t
      =
      { case_insensitive = not case_sensitive
      ; dot_nl
      ; encoding
      ; literal
      ; log_errors
      ; longest_match
      ; never_capture
      ; never_nl
      ; one_line
      ; perl_classes
      ; posix_syntax
      ; word_boundary
      }
    ;;

    let of_serialization
      ({ case_insensitive
       ; dot_nl
       ; encoding
       ; literal
       ; log_errors
       ; longest_match
       ; never_capture
       ; never_nl
       ; one_line
       ; perl_classes
       ; posix_syntax
       ; word_boundary
       } :
        Serialization.t)
      =
      { case_sensitive = not case_insensitive
      ; dot_nl
      ; encoding
      ; literal
      ; log_errors
      ; longest_match
      ; max_mem = default_max_mem
      ; never_capture
      ; never_nl
      ; one_line
      ; perl_classes
      ; posix_syntax
      ; word_boundary
      }
    ;;

    let sexp_of_t t = Serialization.sexp_of_t (to_serialization t)
    let t_of_sexp sexp = of_serialization (Serialization.t_of_sexp sexp)
    let default () = to_serialization default
    let is_default t = [%compare.equal: Serialization.t] (to_serialization t) (default ())

    include
      Core.Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Serialization)
        (struct
          type nonrec t = t

          let to_binable = to_serialization
          let of_binable = of_serialization
        end)

    (* This check verifies the default value produces '()',
       acknowledging that the fields that we believe are default in C code, are
       coded as default in the sexp as well. If this changes, a new stable type
       should be created *)
    let%expect_test _ =
      [%sexp_of: Serialization.t] (default ()) |> print_s;
      [%expect {| () |}]
    ;;
  end
end
