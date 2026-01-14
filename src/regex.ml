(** N.B. when I say "[x] is a convenience function around [y]", that just means [x] can be
    thought of in terms of [y]. In fact, [x] may not be implemented on top of [y], because
    in many cases (e.g., find/find_all) the convenience functions assume certain defaults
    that make it more efficient to drop down into C directly. *)

open Core

type t

let without_trailing_none = Fn.id

module Options = Options

external cre2__init : unit -> unit = "mlre2__init"
external cre2__create_re : Options.Private.C_repr.t -> string -> t = "mlre2__create_re"
external cre2__num_submatches : t -> int = "mlre2__num_submatches" [@@noalloc]
external cre2__submatch_index : t -> string -> int = "mlre2__submatch_index" [@@noalloc]

external cre2__get_named_capturing_groups
  :  t
  -> (string * int) list
  = "mlre2__get_named_capturing_groups"

external cre2__pattern : t -> string = "mlre2__pattern"
external cre2__options : t -> Options.Private.C_repr.t = "mlre2__options"

external cre2__iter_next
  :  t
  -> int
  -> int
  -> string
  -> int * (int * int) option array option
  = "mlre2__iter_next"

external cre2__matches : t -> string -> bool = "mlre2__matches" [@@noalloc]

external cre2__matches_bigstring : t -> Bigstring.t -> bool = "mlre2__matches_bigstring"
[@@noalloc]

(* Unsafe because we don't do any bound checking. *)
external cre2__matches_substring_no_context_unsafe
  :  t
  -> string
  -> pos:int
  -> len:int
  -> bool
  = "mlre2__matches_substring_no_context_unsafe"
[@@noalloc]

external cre2__find_all : t -> int -> string -> string list = "mlre2__find_all"
external cre2__find_first : t -> int -> string -> string = "mlre2__find_first"
external cre2__rewrite_exn : t -> string -> string -> string = "mlre2__rewrite_exn"

external cre2__valid_rewrite_template
  :  t
  -> string
  -> bool
  = "mlre2__valid_rewrite_template"
[@@noalloc]

external cre2__escape : string -> string = "mlre2__escape"

type multiple

external cre2__multiple_create
  :  Options.Private.C_repr.t
  -> multiple
  = "mlre2__multiple_create"

external cre2__multiple_add : multiple -> string -> int = "mlre2__multiple_add"
external cre2__multiple_compile : multiple -> unit = "mlre2__multiple_compile"
external cre2__multiple_match : multiple -> string -> int array = "mlre2__multiple_match"

type regex = t

module Exceptions = struct
  (** [Regex_no_such_subpattern (n, max)] means [n] was requested but only [max]
      subpatterns are defined (so [max] - 1 is the highest valid index) *)
  exception Regex_no_such_subpattern of int * int

  (** [Regex_no_such_named_subpattern (name, pattern)] *)
  exception Regex_no_such_named_subpattern of string * string

  (** [Match_failed pattern] *)
  exception Regex_match_failed of string

  (** [Regex_submatch_did_not_capture (s, i)] means the [i]th subpattern in the regex
      compiled from [s] did not capture a substring. *)
  exception Regex_submatch_did_not_capture of string * int

  (** the string is the C library's error message, generally in the form of
      "(human-readable error): (piece of pattern that did not compile)" *)
  exception Regex_compile_failed of string

  (** [Regex_rewrite_template_invalid (template, error_msg)] *)
  exception Regex_rewrite_template_invalid of string * string

  let () =
    (* register exceptions *)
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_no_such_subpattern"
      (Regex_no_such_subpattern (-1, -1));
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_no_such_named_subpattern"
      (Regex_no_such_named_subpattern ("foo", "bar"));
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_match_failed"
      (Regex_match_failed "");
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_submatch_did_not_capture"
      (Regex_submatch_did_not_capture ("", 0));
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_compile_failed"
      (Regex_compile_failed "");
    (Callback.register_exception [@ocaml.alert "-unsafe_multidomain"])
      "mlre2__Regex_rewrite_template_invalid"
      (Regex_rewrite_template_invalid ("", ""))
  ;;
end

include Exceptions

let () = cre2__init () (* register custom operations *)

let create_exn ?(options = Options.default) pat =
  cre2__create_re (Options.Private.to_c_repr options) pat
;;

let create ?options pat = Or_error.try_with (fun () -> create_exn ?options pat)
let num_submatches t = cre2__num_submatches t

let get_named_capturing_groups t =
  String.Map.of_alist_exn (cre2__get_named_capturing_groups t)
;;

let%template[@alloc a = (heap, stack)] pattern t = cre2__pattern t [@exclave_if_stack a]
let options t = cre2__options t |> Options.Private.of_c_repr
let of_string pat = create_exn pat
let to_string t = cre2__pattern t

module Stable = struct
  open Core.Core_stable

  module V2 = struct
    module Repr = struct
      type t =
        { pattern : string
        ; options : Options.Stable.V2.t
        }
      [@@deriving bin_io, compare ~localize, hash, stable_witness]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5081a6119bfacbe1515e8caf368f40e5 |}]
      ;;

      type t_long_sexp_serialization = string * Options.Stable.V2.t
      [@@deriving sexp ~stackify]

      let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_t
        { pattern; options }
        =
        (if (* in the vast majority of cases, [t] is created with default options,
               therefore we would like to treat that case with just a simple Sexp.Atom
               (more readable in sexp representation) *)
            Options.Stable.V2.is_default options
         then Sexp.V1.Atom pattern
         else [%sexp ((pattern, options) : t_long_sexp_serialization)] [@alloc a])
        [@exclave_if_stack a]
      ;;

      let t_of_sexp = function
        | Sexp.V1.Atom pattern ->
          let options = Options.default in
          { pattern; options }
        | sexp ->
          let pattern, options = [%of_sexp: t_long_sexp_serialization] sexp in
          { pattern; options }
      ;;
    end

    module T = struct
      type nonrec t = t

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "1d372eb2-6c4e-11eb-bd12-aa000016704e"
      ;;

      let%template[@alloc a @ m = (heap_global, stack_local)] to_repr t =
        { Repr.pattern = pattern t; options = options t } [@exclave_if_stack a]
      ;;

      let of_repr { Repr.pattern; options } = create_exn ~options pattern
      let to_binable = to_repr
      let of_binable = of_repr
      let%template[@alloc a = (heap, stack)] to_sexpable = (to_repr [@alloc a])
      let of_sexpable = of_repr

      let stable_witness =
        Stable_witness.of_serializable Repr.stable_witness of_repr to_repr
      ;;
    end

    include T

    module T_serializable_comparable = struct
      include Binable.Of_binable.V2 (Repr) (T)

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b22d9edbef943331b08f0d5df92d4b75 |}]
      ;;

      include Sexpable.Of_sexpable.V1 (Repr) (T)

      include%template Sexpable.Of_sexpable.V1 [@alloc stack] (Repr) (T)

      let compare t1 t2 = Repr.compare (T.to_repr t1) (T.to_repr t2)

      include (val Comparator.V1.make ~compare ~sexp_of_t)
    end

    include T_serializable_comparable

    let hash t = Repr.hash (T.to_repr t)
    let hash_fold_t state t = Repr.hash_fold_t state (T.to_repr t)

    include Comparable.V1.With_stable_witness.Make (struct
        include T
        include T_serializable_comparable
      end)
  end

  module V1_no_options = struct
    module T = struct
      type nonrec t = t

      (* Assert stability here since we depend on the underlying stability of the C
         library for serialization stability. *)
      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      let of_string pat = create_exn ~options:Options.default pat

      let%template[@alloc a = (heap, stack)] to_string t =
        (pattern [@alloc a]) t [@exclave_if_stack a]
      ;;
    end

    include T

    module TS = struct
      include Binable.Of_stringable.V1 [@alert "-legacy"] (T)

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;

      include Sexpable.Of_stringable.V1 (T)

      include%template Sexpable.Of_stringable.V1 [@alloc stack] (T)

      let compare t1 t2 = String.V1.compare (to_string t1) (to_string t2)
      let hash t = String.V1.hash (to_string t)
      let hash_fold_t s t = String.V1.hash_fold_t s (to_string t)
    end

    include TS
  end
end

include Stable.V2.T_serializable_comparable

type id_t =
  [ `Index of int
  | `Name of string
  ]

let index_of_id_exn t = function
  | `Index i ->
    let max = num_submatches t in
    if i < 0 || i > max then raise (Regex_no_such_subpattern (i, max)) else i
  | `Name name ->
    let i = cre2__submatch_index t name in
    if i < 0 || i > num_submatches t
    then raise (Regex_no_such_named_subpattern (name, pattern t))
    else i
;;

module Match = struct
  type t =
    { rex : (regex[@sexp.opaque])
    ; input : string
    ; captures : (int * int) option array
    }
  [@@deriving sexp_of ~stackify]

  let get_pos_exn ~sub t =
    let i = index_of_id_exn t.rex sub in
    let length = Array.length t.captures in
    if i < 0 || i >= length
    then raise (Regex_no_such_subpattern (i, length))
    else (
      match t.captures.(i) with
      | None -> raise (Regex_submatch_did_not_capture (cre2__pattern t.rex, i))
      | Some retval -> retval)
  ;;

  let get_exn ~sub t =
    let pos, len = get_pos_exn ~sub t in
    String.sub t.input ~pos ~len
  ;;

  let get ~sub t =
    Option.map
      (try t.captures.(index_of_id_exn t.rex sub) with
       | _ -> None)
      ~f:(fun (pos, len) -> String.sub t.input ~pos ~len)
  ;;

  let get_all { captures; input; rex = _ } =
    Array.map captures ~f:(Option.map ~f:(fun (pos, len) -> String.sub input ~pos ~len))
  ;;

  (* not exposed in mli *)
  let create ~rex captures ~input = { rex; input; captures }
end

let to_sequence_exn ?sub t input =
  let n =
    match sub with
    | None -> -1
    | Some (`Index n) -> if n >= 0 then n else 0
    | Some (`Name _ as name) -> index_of_id_exn t name
  in
  Sequence.unfold ~init:0 ~f:(fun pos ->
    if pos < 0
    then None
    else (
      let pos, matches = cre2__iter_next t pos n input in
      Option.map matches ~f:(fun m -> Match.create ~rex:t ~input m, pos)))
;;

let find_all_exn ?(sub = `Index 0) t input =
  cre2__find_all t (index_of_id_exn t sub) input
;;

let find_all ?sub t input = Or_error.try_with (fun () -> find_all_exn ?sub t input)

let find_first_exn ?(sub = `Index 0) t input =
  cre2__find_first t (index_of_id_exn t sub) input
;;

let find_first ?sub t input = Or_error.try_with (fun () -> find_first_exn ?sub t input)

let find_submatches_exn t input =
  let n = num_submatches t in
  let seq = to_sequence_exn ~sub:(`Index n) t input in
  let matches =
    match Sequence.next seq with
    | None -> raise (Regex_match_failed (cre2__pattern t))
    | Some (m, _) -> m
  in
  Array.init n ~f:(fun i -> Match.get ~sub:(`Index i) matches)
;;

let find_submatches t input = Or_error.try_with (fun () -> find_submatches_exn t input)
let matches t input = cre2__matches t input
let matches_bigstring t input = cre2__matches_bigstring t input

let matches_substring_no_context_exn t input ~pos ~len =
  let input_length = String.length input in
  if pos < 0 || len < 0 || pos > input_length - len
  then
    raise_s
      [%message
        "Matches_substring_no_context_exn expects pos >= 0, len >= 0, and pos + len <= \
         String.length input"
          (pos : int)
          (len : int)
          (input_length : int)];
  cre2__matches_substring_no_context_unsafe t input ~pos ~len
;;

let get_matches_exn ?sub ?max t input =
  let seq = to_sequence_exn ?sub t input in
  let seq =
    match max with
    | None -> seq
    | Some limit -> Sequence.take seq limit
  in
  Sequence.to_list seq
;;

let get_matches ?sub ?max t input =
  Or_error.try_with (fun () -> get_matches_exn ?sub ?max t input)
;;

let first_match_exn t input = List.hd_exn (get_matches_exn t input ~max:1)
let first_match t input = Or_error.try_with (fun () -> first_match_exn t input)

module Substring = struct
  type t =
    { src : string
    ; src_pos : int
    ; len : int
    }

  let create ~pos ~len src = { src; src_pos = pos; len }
  let of_string src = { src; src_pos = 0; len = String.length src }

  let concat_string ~len substrings : string =
    let dst = Bytes.create len in
    ignore
      (List.fold_left substrings ~init:0 ~f:(fun dst_pos { src; src_pos; len } ->
         Bytes.From_string.blit ~src ~src_pos ~dst ~dst_pos ~len;
         dst_pos + len)
       : int);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
  ;;
end

module Return = struct
  let substrings str (pos, len) = Substring.create ~pos ~len str
  let strings src (src_pos, len) = String.sub src ~pos:src_pos ~len
end

let split_internal ?(include_matches = false) return input (matches : Match.t list) =
  (* if additional speed is needed, maybe try optimizing away the closures *)
  let gaps ~pos ~acc ~pos' ~len':_ = return input (pos, pos' - pos) :: acc in
  let both ~pos ~acc ~pos' ~len' =
    return input (pos', len') :: return input (pos, pos' - pos) :: acc
  in
  let f g (pos, acc) m =
    let pos', len' = Match.get_pos_exn ~sub:(`Index 0) m in
    pos' + len', g ~pos ~acc ~pos' ~len'
  in
  let last_sep, acc =
    List.fold_left ~init:(0, []) matches ~f:(if include_matches then f both else f gaps)
  in
  List.rev (return input (last_sep, String.length input - last_sep) :: acc)
;;

let split ?max ?(include_matches = false) t input =
  let matches = get_matches_exn ?max ~sub:(`Index 1) t input in
  split_internal ~include_matches Return.strings input matches
;;

let replace_exn ?sub ?only ~f t input =
  let only' =
    match only with
    | Some i -> Some (i + 1)
    | None -> None
  in
  let matches = get_matches_exn ?sub ?max:only' t input in
  let gaps = split_internal Return.substrings input matches in
  let replacements =
    let whole_match m = Match.get_exn ~sub:(`Index 0) m in
    let f' f m = Substring.of_string (f m) in
    match only with
    | None -> List.rev_map ~f:(f' f) matches
    | Some to_be_replaced ->
      List.rev_mapi matches ~f:(fun i ->
        f' (if i = to_be_replaced then f else whole_match))
  in
  let rec interleave (len, acc) l = function
    | [] ->
      let len' =
        List.fold_left ~init:0 l ~f:(fun x { Substring.len; src = _; src_pos = _ } ->
          x + len)
      in
      len + len', List.rev (List.rev_append l acc)
    | h :: tl -> interleave (len + h.Substring.len, h :: acc) tl l
  in
  let len, substrings = interleave (0, []) (List.rev replacements) gaps in
  Substring.concat_string ~len substrings
;;

let replace ?sub ?only ~f t input =
  Or_error.try_with (fun () -> replace_exn ?sub ?only ~f t input)
;;

let rewrite_exn t ~template input = cre2__rewrite_exn t input template

let rewrite t ~template input =
  Or_error.try_with (fun () -> rewrite_exn t ~template input)
;;

let valid_rewrite_template t ~template = cre2__valid_rewrite_template t template
let escape input = cre2__escape input

module Multiple = struct
  type 'a t =
    { set : multiple
    ; vals : 'a array
    }

  let create_exn ?(options = Options.default) entries =
    let t =
      { set = cre2__multiple_create (Options.Private.to_c_repr options)
      ; vals = Array.of_list (List.map ~f:snd entries)
      }
    in
    List.iteri entries ~f:(fun expected (pat, _) ->
      let observed = cre2__multiple_add t.set pat in
      if Int.( <> ) expected observed
      then
        raise_s
          [%message
            "cre2__multiple_add returned unexpected index."
              (expected : int)
              (observed : int)]);
    cre2__multiple_compile t.set;
    t
  ;;

  let create ?options entries = Or_error.try_with (fun () -> create_exn ?options entries)

  let values_of_indices t indices =
    Array.fold_right indices ~init:[] ~f:(fun i acc -> t.vals.(i) :: acc)
  ;;

  let matches_no_order t s = values_of_indices t (cre2__multiple_match t.set s)

  let matches t s =
    let indices = cre2__multiple_match t.set s in
    Array.sort indices ~compare:Int.compare;
    values_of_indices t indices
  ;;

  let vals t = Array.Permissioned.of_array t.vals
end

module Infix = struct
  let ( =~ ) input t = matches t input
end

module%test _ = struct
  let%test _ =
    let re = create_exn "^(.*)\\\\" in
    let buf = Bin_prot.Common.create_buf 100 in
    ignore (Stable.V1_no_options.bin_write_t buf ~pos:0 re : int);
    Int.( = ) 0 (compare re (Stable.V1_no_options.bin_read_t buf ~pos_ref:(ref 0)))
  ;;

  let%test _ =
    let re =
      create_exn ~options:{ Options.default with case_sensitive = false } "^(.*)\\\\"
    in
    let buf = Bin_prot.Common.create_buf 100 in
    ignore (Stable.V2.bin_write_t buf ~pos:0 re : int);
    Int.( = ) 0 (Stable.V2.compare re (Stable.V2.bin_read_t buf ~pos_ref:(ref 0)))
  ;;

  let%test _ =
    let re = create_exn "^(.*)\\\\" in
    Int.( = ) 0 (compare re (Stable.V2.t_of_sexp (sexp_of_t re)))
  ;;

  let%test _ =
    let foo, a_star, dot_capture = create_exn "foo", create_exn "a*", create_exn "(.)" in
    let ( < ) a b = compare a b < 0 in
    a_star < foo && dot_capture < a_star && dot_capture < foo
  ;;

  let%test_unit _ =
    let re = create_exn "^" in
    match get_matches_exn re "XYZ" with
    | [ the_match ] ->
      [%test_eq: int * int] (0, 0) (Match.get_pos_exn ~sub:(`Index 0) the_match)
    | other -> raise_s [%sexp "expected exactly one match", (other : Match.t list)]
  ;;

  let%test_unit _ =
    let re = create_exn "^" in
    [%test_eq: string] "aXYZ" (replace_exn re "XYZ" ~f:(const "a"))
  ;;
end

let%bench_fun ("find_submatches with many Nones" [@indexed n = [ 5; 10; 50; 100; 200 ]]) =
  let regex =
    "^"
    ^ String.concat ~sep:"|" (List.init n ~f:(fun i -> "(" ^ Int.to_string i ^ ")"))
    ^ "$"
    |> create_exn
  in
  fun () ->
    let _r = find_submatches regex (Int.to_string n) in
    ()
;;

let%expect_test "roundtrip Stable.V2.t_of_sexp and Stable.V2.sexp_of_t" =
  [ {|""|}
  ; "^sim?ple*"
  ; "(cAse ((case_insensitive)))"
  ; "(cAse ((case_insensitive) (encoding Latin1)))"
  ]
  |> List.iter ~f:(fun s ->
    Sexp.of_string_conv_exn s Stable.V2.t_of_sexp |> Stable.V2.sexp_of_t |> print_s);
  [%expect
    {|
    ""
    ^sim?ple*
    (cAse ((case_insensitive)))
    (cAse ((case_insensitive) (encoding Latin1)))
    |}]
;;

let%expect_test "behavior of options wrt comparison/hashing" =
  let t1 x = create_exn ~options:{ Options.default with case_sensitive = true } x in
  let t2 x = create_exn ~options:{ Options.default with case_sensitive = false } x in
  (let t1 = t1 ""
   and t2 = t2 "" in
   assert (not ([%compare.equal: t] t1 t2));
   assert ([%compare.equal: Stable.V1_no_options.t] t1 t2);
   assert (not ([%compare.equal: Stable.V2.t] t1 t2)));
  let stable_v2_unequal =
    List.filter [ ""; "1"; "2"; "3" ] ~f:(fun str ->
      let h hash = hash (t1 str) = hash (t2 str) in
      assert (h [%hash: Stable.V1_no_options.t]);
      not (h [%hash: Stable.V2.t]))
  in
  assert (not (List.is_empty stable_v2_unequal))
;;

let%test_unit "t preserved via Stable.V2.sexp_of_t and Stable.V2.t_of_sexp" =
  let f_pattern = pattern
  and f_options = options in
  List.iter
    [ "", None
    ; "^sim?ple*", None
    ; "cAse", Some { Options.default with case_sensitive = false }
    ; "cAse", Some { Options.default with case_sensitive = false; encoding = Latin1 }
    ]
    ~f:(fun (pattern, options) ->
      let t = create_exn ?options pattern |> Stable.V2.sexp_of_t |> Stable.V2.t_of_sexp in
      let options = Option.value options ~default:Options.default in
      [%test_eq: string * Options.t] (f_pattern t, f_options t) (pattern, options))
;;

include Comparable.Make_plain_using_comparator (struct
    type nonrec t = t

    include Stable.V2.T_serializable_comparable
  end)

include Hashable.Make_plain (struct
    type nonrec t = t

    include Stable.V2.T_serializable_comparable

    let hash = Stable.V2.hash
    let hash_fold_t = Stable.V2.hash_fold_t
  end)
