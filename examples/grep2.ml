open StdLabels
module Regex = Re2.Regex

let underline_on = Format.sprintf "%c[4m" (Char.chr 27)
let underline_off = Format.sprintf "%c[24m" (Char.chr 27)

let usage = Format.sprintf "%s [options] pattern" (Array.get Sys.argv 0)

let pattern = ref ""
let rewrite = ref "\\0"
let underline = ref false
let only_matching = ref false
let str_sub = ref ""

(* this is just drudgery *)
let () =
  Arg.parse [
    "--underline", Arg.Set underline, "underline rewrites";
    "--rewrite", Arg.Set_string rewrite, "rewrite the match";
    "--only-matching", Arg.Set only_matching, "only print the match, not the line";
    "--extract-submatch", Arg.Set_string str_sub,
      "ignored unless --only-matching is also passed; whole match is submatch 0";
  ]
  (fun pattern' -> pattern := pattern')
  usage
;;

(* this is the only interesting (i.e., Re2-using) part *)
let () =
  if !underline then
    let re = Regex.create_exn "\\\\[0-9]" in
    let template = Format.sprintf "%c[4m\\0%c[24m" (Char.chr 27) (Char.chr 27) in
    match  (Regex.rewrite re ~template !rewrite) with
    | Core.Std.Result.Ok s -> rewrite := s
    | Core.Std.Result.Error _ -> ()
;;

let re =
  if !pattern = ""
  then raise (Failure (Format.sprintf "invalid pattern /%s/" !pattern))
  else begin
    try
      Regex.create_exn !pattern
    with
    | _ ->
      raise (Failure (Format.sprintf "invalid pattern /%s/" !pattern))
  end
;;

let sub =
  if !str_sub = "" then `Index 0
  else let id = try `Index (int_of_string !str_sub) with _ -> `Name !str_sub in
  `Index (Regex.index_of_id_exn re id)
;;

let grep str =
  try
    if not (Regex.matches re str) then None else
      let str = if not !only_matching then str else Regex.find_first_exn ~sub re str in
      Some (Regex.rewrite re ~template:(!rewrite) str)
  with
  | err -> raise err
;;

(* okay, more drudgery *)
let _ =
  let rec iter ~unfold ~fold ~init seed =
    match unfold seed with
    | None -> init
    | Some (x, seed) -> iter ~unfold ~fold ~init:(fold init x) seed
  in
  iter
  ~unfold:(fun channel ->
    match
      try grep (input_line channel) with
      | End_of_file -> None
      | err -> raise err
    with
    | None -> None
    | Some r -> Some (r, channel))
  ~fold:(fun channel str_result ->
    match str_result with
    | Core.Std.Result.Error _ -> channel
    | Core.Std.Result.Ok str -> output_string channel (str ^ "\n") ; channel)
  ~init:stdout
  stdin
;;

