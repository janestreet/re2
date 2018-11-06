open! Core_kernel

type t = [
  | `Case_sensitive of bool
  | `Dot_nl of bool
  | `Encoding_latin1 of bool
  | `Literal of bool
  | `Log_errors of bool
  | `Longest_match of bool
  | `Max_mem of int
  | `Never_capture of bool
  | `Never_nl of bool
  | `One_line of bool
  | `Perl_classes of bool
  | `Posix_syntax of bool
  | `Word_boundary of bool
]
[@@deriving compare, sexp_of]

let latin1 = [ `Encoding_latin1 true; ]

let posix = [ `Posix_syntax true; `Longest_match true; ]

let noisy = [ `Log_errors true; ]

module Encoding = struct

  type t =
    | Latin1
    | Utf8
  [@@deriving compare, equal]

  module C_repr = struct
    type t = int [@@deriving compare, sexp_of]
    let equal = Int.(=) (* would use [@@deriving equal], but equal_int is not in scope *)

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

module C_repr = struct
  type t

  (*$ #use "options.cinaps";;
    List.iter all ~f:(fun { name; type_ = { ocaml_type; _} } ->
    printf "\n  external %s : t -> %s = \"mlre2__options__%s\" [@@noalloc]\n"
    name ocaml_type name;
    printf "  external set_%s : t -> %s -> unit = \"mlre2__options__set_%s\" [@@noalloc]\n"
    name ocaml_type name);

    printf "  "
  *)
  external case_sensitive : t -> bool = "mlre2__options__case_sensitive" [@@noalloc]
  external set_case_sensitive : t -> bool -> unit = "mlre2__options__set_case_sensitive" [@@noalloc]

  external dot_nl : t -> bool = "mlre2__options__dot_nl" [@@noalloc]
  external set_dot_nl : t -> bool -> unit = "mlre2__options__set_dot_nl" [@@noalloc]

  external encoding : t -> Encoding.C_repr.t = "mlre2__options__encoding" [@@noalloc]
  external set_encoding : t -> Encoding.C_repr.t -> unit = "mlre2__options__set_encoding" [@@noalloc]

  external literal : t -> bool = "mlre2__options__literal" [@@noalloc]
  external set_literal : t -> bool -> unit = "mlre2__options__set_literal" [@@noalloc]

  external log_errors : t -> bool = "mlre2__options__log_errors" [@@noalloc]
  external set_log_errors : t -> bool -> unit = "mlre2__options__set_log_errors" [@@noalloc]

  external longest_match : t -> bool = "mlre2__options__longest_match" [@@noalloc]
  external set_longest_match : t -> bool -> unit = "mlre2__options__set_longest_match" [@@noalloc]

  external max_mem : t -> int = "mlre2__options__max_mem" [@@noalloc]
  external set_max_mem : t -> int -> unit = "mlre2__options__set_max_mem" [@@noalloc]

  external never_capture : t -> bool = "mlre2__options__never_capture" [@@noalloc]
  external set_never_capture : t -> bool -> unit = "mlre2__options__set_never_capture" [@@noalloc]

  external never_nl : t -> bool = "mlre2__options__never_nl" [@@noalloc]
  external set_never_nl : t -> bool -> unit = "mlre2__options__set_never_nl" [@@noalloc]

  external one_line : t -> bool = "mlre2__options__one_line" [@@noalloc]
  external set_one_line : t -> bool -> unit = "mlre2__options__set_one_line" [@@noalloc]

  external perl_classes : t -> bool = "mlre2__options__perl_classes" [@@noalloc]
  external set_perl_classes : t -> bool -> unit = "mlre2__options__set_perl_classes" [@@noalloc]

  external posix_syntax : t -> bool = "mlre2__options__posix_syntax" [@@noalloc]
  external set_posix_syntax : t -> bool -> unit = "mlre2__options__set_posix_syntax" [@@noalloc]

  external word_boundary : t -> bool = "mlre2__options__word_boundary" [@@noalloc]
  external set_word_boundary : t -> bool -> unit = "mlre2__options__set_word_boundary" [@@noalloc]
  (*$*)

  external create_quiet : unit -> t = "mlre2__options__create_quiet"


end

let to_c_repr ts =
  let c_repr = C_repr.create_quiet () in
  List.iter ts ~f:(function
    | `Encoding_latin1 true -> C_repr.set_encoding c_repr (Encoding.to_c_repr Latin1)
    | `Encoding_latin1 false -> C_repr.set_encoding c_repr (Encoding.to_c_repr Utf8)
    (*$ List.iter all_but_encoding ~f:(fun { name; type_ = _} ->
      printf "\n    | `%s b -> C_repr.set_%s c_repr b"
      (String.capitalize_ascii name) name) *)
    | `Case_sensitive b -> C_repr.set_case_sensitive c_repr b
    | `Dot_nl b -> C_repr.set_dot_nl c_repr b
    | `Literal b -> C_repr.set_literal c_repr b
    | `Log_errors b -> C_repr.set_log_errors c_repr b
    | `Longest_match b -> C_repr.set_longest_match c_repr b
    | `Max_mem b -> C_repr.set_max_mem c_repr b
    | `Never_capture b -> C_repr.set_never_capture c_repr b
    | `Never_nl b -> C_repr.set_never_nl c_repr b
    | `One_line b -> C_repr.set_one_line c_repr b
    | `Perl_classes b -> C_repr.set_perl_classes c_repr b
    | `Posix_syntax b -> C_repr.set_posix_syntax c_repr b
    | `Word_boundary b -> C_repr.set_word_boundary c_repr b(*$*));
  c_repr
;;

let of_c_repr c_repr =
  [ (*$ List.iter all_but_encoding ~f:(fun { name; type_ = _} ->
      printf "\n    `%s (C_repr.%s c_repr);" (String.capitalize_ascii name) name) *)
    `Case_sensitive (C_repr.case_sensitive c_repr);
    `Dot_nl (C_repr.dot_nl c_repr);
    `Literal (C_repr.literal c_repr);
    `Log_errors (C_repr.log_errors c_repr);
    `Longest_match (C_repr.longest_match c_repr);
    `Max_mem (C_repr.max_mem c_repr);
    `Never_capture (C_repr.never_capture c_repr);
    `Never_nl (C_repr.never_nl c_repr);
    `One_line (C_repr.one_line c_repr);
    `Perl_classes (C_repr.perl_classes c_repr);
    `Posix_syntax (C_repr.posix_syntax c_repr);
    `Word_boundary (C_repr.word_boundary c_repr);(*$*)
    `Encoding_latin1 (Encoding.equal Latin1 (C_repr.encoding c_repr |> Encoding.of_c_repr));
  ]
;;

module Private = struct
  let examples_for_testing =
    let all_of_int = [ 10_000 ] in
    let module T = struct
      type t = [
        | `Case_sensitive of bool
        | `Dot_nl of bool
        | `Encoding_latin1 of bool
        | `Literal of bool
        | `Log_errors of bool
        | `Longest_match of bool
        | `Max_mem of int
        | `Never_capture of bool
        | `Never_nl of bool
        | `One_line of bool
        | `Perl_classes of bool
        | `Posix_syntax of bool
        | `Word_boundary of bool
      ]
      [@@deriving enumerate]
    end in
    T.all
  ;;
end
