(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    rule "Generate lib/options.ml"
      ~prod:"lib/options.ml"
      ~deps:["lib/options.mlp"; "lib/enum_x_macro.h"]
      (fun _ _ ->
        Cmd (S[A"cpp"; A"-o"; P"lib/options.ml"; P"lib/options.mlp"]));
    
    flag ["ocaml"; "link"; "library"; "native"] (S[A"-cclib"; A"-Llib";
                                                   A"-cclib"; A"-lre2_stubs"]);
    flag ["ocaml"; "link"; "library"; "byte"]   (S[A"-dllib"; A"dllre2_stubs.so"]);
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook);;
