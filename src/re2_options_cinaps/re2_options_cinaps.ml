(* -*- mode:tuareg -*- *)

include StdLabels
include Printf

module Type = struct
  type t =
    { ocaml_type : string
    ; value_of_c : string
    ; value_to_c : string
    }

  let bool = { ocaml_type = "bool"; value_of_c = "Val_bool"; value_to_c = "Bool_val" }
  let int = { ocaml_type = "int"; value_of_c = "Val_int"; value_to_c = "Int_val" }

  let encoding =
    { ocaml_type = "Encoding.C_repr.t"
    ; value_of_c = "Val_int"
    ; value_to_c = "static_cast<RE2::Options::Encoding>Int_val"
    }
  ;;
end

type t =
  { name : string
  ; type_ : Type.t
  }

let all =
  [ { name = "case_sensitive"; type_ = Type.bool }
  ; { name = "dot_nl"; type_ = Type.bool }
  ; { name = "encoding"; type_ = Type.encoding }
  ; { name = "literal"; type_ = Type.bool }
  ; { name = "log_errors"; type_ = Type.bool }
  ; { name = "longest_match"; type_ = Type.bool }
  ; { name = "max_mem"; type_ = Type.int }
  ; { name = "never_capture"; type_ = Type.bool }
  ; { name = "never_nl"; type_ = Type.bool }
  ; { name = "one_line"; type_ = Type.bool }
  ; { name = "perl_classes"; type_ = Type.bool }
  ; { name = "posix_syntax"; type_ = Type.bool }
  ; { name = "word_boundary"; type_ = Type.bool }
  ]
;;

let print_c_repr_external_bindings () =
  List.iter all ~f:(fun { name; type_ = { ocaml_type; _ } } ->
    print_string
      [%string
        {|
external %{name} : t -> %{ocaml_type} = "mlre2__options__%{name}" [@@noalloc]
external set_%{name} : t -> %{ocaml_type} -> unit = "mlre2__options__set_%{name}" [@@noalloc]
|}])
;;

let print_to_c_repr_fields () =
  List.iter all ~f:(fun { name; type_ = _ } ->
    match name with
    | "encoding" ->
      print_endline
        "~encoding:(f (fun c_repr value -> C_repr.set_encoding c_repr \
         (Encoding.to_c_repr value)))"
    | _ -> print_endline [%string {|~%{name}:(f C_repr.set_%{name})|}])
;;

let print_of_c_repr_fields () =
  List.iter all ~f:(fun { name; type_ = _ } ->
    if String.equal "encoding" name
    then
      printf
        "\n~encoding:(f (fun c_repr -> Encoding.of_c_repr (C_repr.encoding c_repr)))"
    else printf "\n~%s:(f C_repr.%s)" name name)
;;

let print_c_prototypes () =
  List.iter all ~f:(fun { name; _ } ->
    print_string
      [%string
        {|
  extern value mlre2__options__%{name}(value v_options);
  extern value mlre2__options__set_%{name}(value v_options, value v_value);
|}])
;;

let print_c_stubs () =
  List.iter all ~f:(fun { name; type_ = { value_of_c; value_to_c; _ } } ->
    print_string
      [%string
        {|
  CAMLprim value mlre2__options__%{name}(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(%{value_of_c}(options->%{name}()));
  }

  CAMLprim value mlre2__options__set_%{name}(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_%{name}(%{value_to_c}(v_value));
    CAMLreturn(Val_unit);
  }
|}])
;;
