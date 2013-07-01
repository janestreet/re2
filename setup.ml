(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
open OASISTypes;;
let () =
  let ret = Sys.command "./_prebuild" in
  if ret <> 0 then exit ret
;;
(* OASIS_STOP *)
let () = setup ()
