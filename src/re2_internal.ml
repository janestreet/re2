
(** N.B. when I say "[x] is a convenience function around [y]", that just means [x] can be
    thought of in terms of [y].  In fact, [x] may not be implemented on top of [y], because
    in many cases (e.g., find/find_all) the convenience functions assume certain defaults
    that make it more efficient to drop down into C directly.
*)

open Core_kernel

type t

type 'a without_trailing_none = 'a [@@deriving sexp_of]

let without_trailing_none = Fn.id

module Options = Options

external cre2__init : unit -> unit = "mlre2__init"
external cre2__create_re : Options.enum list -> string -> t = "mlre2__create_re"
external cre2__num_submatches : t -> int = "mlre2__num_submatches" [@@noalloc]

external cre2__submatch_index : t -> string -> int = "mlre2__submatch_index" [@@noalloc]

external cre2__pattern : t -> string = "mlre2__pattern"

external cre2__iter_next :
  t
  -> int
  -> int
  -> string
  -> (int * (int * int) option array option)
  = "mlre2__iter_next"

external cre2__matches : t -> string -> bool = "mlre2__matches" [@@noalloc]
external cre2__find_all : t -> int -> string -> string list = "mlre2__find_all"
external cre2__find_first : t -> int -> string -> string = "mlre2__find_first"
external cre2__rewrite_exn : t -> string -> string -> string = "mlre2__rewrite_exn"
external cre2__valid_rewrite_template : t -> string -> bool =
  "mlre2__valid_rewrite_template" [@@noalloc]
external cre2__escape : string -> string = "mlre2__escape"

type regexset
external cre2__create_set  : Options.enum list -> regexset = "mlre2__create_set"
external cre2__set_add     : regexset -> string -> int = "mlre2__set_add"
external cre2__set_compile : regexset -> bool = "mlre2__set_compile"
external cre2__set_match   : regexset -> string -> int array = "mlre2__set_match"

type regex = t

module Exceptions = struct
  exception Regex_no_such_subpattern of int * int
  (** [Regex_no_such_subpattern (n, max)] means [n] was requested but only [max]
      subpatterns are defined (so [max] - 1 is the highest valid index) *)
  exception Regex_no_such_named_subpattern of string * string
  (** [Regex_no_such_named_subpattern (name, pattern)] *)
  exception Regex_match_failed of string
  (** [Match_failed pattern] *)
  exception Regex_submatch_did_not_capture of string * int
  (** [Regex_submatch_did_not_capture (s, i)] means the [i]th subpattern in the
      regex compiled from [s] did not capture a substring. *)
  exception Regex_compile_failed of string
  (** the string is the C library's error message, generally in the form of
      "(human-readable error): (piece of pattern that did not compile)" *)
  exception Regex_rewrite_template_invalid of string * string
  (** [Regex_rewrite_template_invalid (template, error_msg)] *)

  let _ = (* register exceptions *)
    Callback.register_exception "mlre2__Regex_no_such_subpattern"
      (Regex_no_such_subpattern (-1, -1));
    Callback.register_exception "mlre2__Regex_no_such_named_subpattern"
      (Regex_no_such_named_subpattern ("foo", "bar"));
    Callback.register_exception "mlre2__Regex_match_failed"
      (Regex_match_failed "");
    Callback.register_exception "mlre2__Regex_submatch_did_not_capture"
      (Regex_submatch_did_not_capture ("", 0));
    Callback.register_exception "mlre2__Regex_compile_failed"
      (Regex_compile_failed "");
    Callback.register_exception "mlre2__Regex_rewrite_template_invalid"
      (Regex_rewrite_template_invalid ("", ""));
  ;;
end
include Exceptions

let () = cre2__init ()    (* register custom operations *)

let create_exn ?(options=[]) pat =
  cre2__create_re (List.map ~f:Options.enum_of_t options) pat
;;

let create ?options pat = Or_error.try_with (fun () -> create_exn ?options pat)

let num_submatches t          = cre2__num_submatches t
let pattern t                 = cre2__pattern t

let of_string pat = create_exn pat
let to_string t   = cre2__pattern t

module T = struct
  type nonrec t = t
  let of_string = of_string
  let to_string = to_string
end

include Binable. Of_stringable (T)
include Sexpable.Of_stringable (T)

let compare t1 t2 = String.compare (to_string t1) (to_string t2)

type id_t = [ `Index of int | `Name of string ]

let index_of_id_exn t = function
  | `Index i ->
    let max = num_submatches t in
    if i < 0 || i > max
    then raise (Regex_no_such_subpattern (i, max))
    else i
  | `Name name ->
    let i = cre2__submatch_index t name in
    if i < 0 || i > num_submatches t
    then raise (Regex_no_such_named_subpattern (name, pattern t))
    else i
;;

module Match = struct
  type t = {
    rex      : regex sexp_opaque;
    input    : string;
    captures : (int * int) option array;
  } [@@deriving sexp_of]

  let get_pos_exn ~sub t =
    let i = index_of_id_exn t.rex sub in
    let maybe_captures =
      try Array.get t.captures i with
      | Invalid_argument "index out of bounds" ->
        raise (Regex_no_such_subpattern (i, Array.length t.captures))
      | e -> raise e
    in
    match maybe_captures with
    | None        -> raise (Regex_submatch_did_not_capture (cre2__pattern t.rex, i))
    | Some retval -> retval
  ;;

  let get_exn ~sub t =
    let pos, len = get_pos_exn ~sub t in
    String.sub t.input ~pos ~len
  ;;

  let get ~sub t =
    Option.map (try Array.get t.captures (index_of_id_exn t.rex sub) with _ -> None)
      ~f:(fun (pos, len) -> String.sub t.input ~pos ~len)

  let get_all { captures; input; rex=_ } =
    Array.map captures ~f:(Option.map ~f:(fun (pos, len) -> String.sub input ~pos ~len))

  (* not exposed in mli *)
  let create ~rex captures ~input = {rex; input; captures; }
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
      Option.map matches ~f:(fun m -> (Match.create ~rex:t ~input m, pos))
    ))
;;

let find_all_exn ?(sub=(`Index 0)) t input =
  cre2__find_all t (index_of_id_exn t sub) input
;;

let find_all ?sub t input = Or_error.try_with (fun () -> find_all_exn ?sub t input)

let find_first_exn ?(sub=(`Index 0)) t input = cre2__find_first t (index_of_id_exn t sub) input

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

let find_submatches t input = Or_error.try_with (fun () -> find_submatches_exn t input)

let matches t input = cre2__matches t input

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

let first_match t input = Or_error.try_with (fun () -> first_match_exn t input )

module Substring = struct

  type t = {
    src     : string;
    src_pos : int;
    len     : int;
  }

  let create ~pos ~len src = {src; src_pos = pos; len}

  let of_string src = {src; src_pos = 0; len = String.length src}

  let concat_string ~len substrings : string =
    let dst = String.create len in
    ignore (List.fold_left substrings ~init:0 ~f:(fun dst_pos {src; src_pos; len} ->
      String.blit ~src ~src_pos ~dst ~dst_pos ~len;
      dst_pos + len) : int);
    dst
  ;;

end

module Return = struct
  let substrings str (pos, len) = Substring.create ~pos ~len str
  let strings src (src_pos, len) =
    let dst = String.create len in
    String.blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    dst
  ;;
end

let split_internal ?(include_matches=false) return input (matches:Match.t list) =
  (* if additional speed is needed, maybe try optimizing away the closures *)
  let gaps ~pos ~acc ~pos' ~len':_ = return input (pos, pos' - pos)::acc in
  let both ~pos ~acc ~pos' ~len' =
    return input (pos', len')::(return input (pos, pos' - pos))::acc
  in
  let f g (pos, acc) m =
    let pos', len' = Match.get_pos_exn ~sub:(`Index 0) m  in
    (pos' + len', g ~pos ~acc ~pos' ~len')
  in
  let last_sep, acc =
    List.fold_left ~init:(0, []) matches ~f:(if include_matches then f both else f gaps)
  in
  List.rev (return input (last_sep, String.length input - last_sep)::acc)
;;

let split ?max ?(include_matches=false) t input =
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
      let len' = List.fold_left ~init:0 l ~f:(fun x {Substring.len; src=_; src_pos=_ } -> x + len) in
      (len + len', List.rev (List.rev_append l acc))
    | h::tl -> interleave (len + h.Substring.len, h::acc) tl l
  in
  let len, substrings = interleave (0, []) (List.rev replacements) gaps in
  Substring.concat_string ~len substrings
;;

let replace ?sub ?only ~f t input =
  Or_error.try_with (fun () -> replace_exn ?sub ?only ~f t input)

let rewrite_exn t ~template input = cre2__rewrite_exn t input template
let rewrite t ~template input =
  Or_error.try_with (fun () -> rewrite_exn t ~template input)

let valid_rewrite_template t ~template = cre2__valid_rewrite_template t template

let escape input = cre2__escape input

module Dict = struct
  type 'a redict = {
    set  : regexset ;
    vals : 'a array ;
  };;

  let make ?options:(opts=[]) entries =
    let res = {set  = cre2__create_set (List.map ~f:Options.enum_of_t opts) ;
	       vals = Array.of_list (List.map ~f:(function (_, v) -> v) entries)} in
    try
      List.iter ~f:(function (pat, _) -> ignore (cre2__set_add res.set pat)) entries ;
      if cre2__set_compile res.set then Some res else None
    with | _ -> None

  let get_all d s =
    let results = cre2__set_match d.set s in
    List.rev (Array.fold ~f:(fun acc -> fun idx -> (d.vals.(idx))::acc)
	       ~init:[] results)

end

module Infix = struct
  let (~/) pat = create_exn pat

  let (//~!) input t = find_first_exn t input
  let (//~)  input t = find_first t input

  let (=~)   input t = matches t input
end

let%test_module _ = (module struct
  let%test _ = begin
    let re = create_exn "^(.*)\\\\" in
    let buf = Bin_prot.Common.create_buf 100 in
    ignore (bin_write_t buf ~pos:0 re : int);
    Int.(=) 0 (compare re (bin_read_t buf ~pos_ref:(ref 0)))
  end

  let%test _ = begin
    let re = create_exn "^(.*)\\\\" in
    Int.(=) 0 (compare re (t_of_sexp (sexp_of_t re)))
  end

  let%test _ = begin
    let foo, a_star, dot_capture = create_exn "foo", create_exn "a*", create_exn "(.)" in
    let (<) a b = compare a b < 0 in
    a_star < foo && dot_capture < a_star && dot_capture < foo
  end

  let%test_unit _ =
    let re = create_exn "^" in
    match get_matches_exn re "XYZ" with
    | [ the_match ] ->
      [%test_eq: int * int] (0, 0) (Match.get_pos_exn ~sub:(`Index 0) the_match)
    | other -> failwiths "expected exactly one match" other [%sexp_of: Match.t list]

  let%test_unit _ =
    let re = create_exn "^" in
    [%test_eq: string] "aXYZ" (replace_exn re "XYZ" ~f:(const "a"))

end)

let%test_module _ = (module struct
  let%test_unit _ = begin
    match Dict.make [("^X",   `Starts_X);
                     ("^Y",   `Starts_Y);
                     (".*X",  `Contains_X);
                     (".*Y",  `Contains_Y);
                     (".*X$", `Ends_X);
                     (".*Y$", `Ends_Y)] with
    | Some d -> [%test_eq: [ `Starts_X | `Starts_Y | `Contains_X | `Contains_Y | `Ends_X | `Ends_Y] list]
                    [] (List.filter (fun x -> not (List.mem x [`Starts_X; `Contains_X; `Contains_Y; `Ends_Y]))
		    (Dict.get_all d "XY")
    | _      -> failwith "failed to compile dictionary"
  end
end)

let%bench_fun "find_submatches with many Nones" [@indexed n = [5;10;50;100;200]] =
  let regex =
    "^" ^
    String.concat ~sep:"|"
      (List.init n ~f:(fun i -> "(" ^ Int.to_string i ^ ")"))
    ^ "$"
    |> create_exn
  in
  (fun () ->
     let _r = find_submatches regex (Int.to_string n) in
     ())
