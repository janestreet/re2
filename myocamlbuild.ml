(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    rule "Generate src/options.ml"
      ~prod:"src/options.ml"
      ~deps:["src/options.mlp"; "src/enum_x_macro.h"]
      (fun _ _ -> Cmd (S[A"cc"; A"-E"; A"-P"; A"-x"; A"c";
                         P"src/options.mlp"; A"-o"; P"src/options.ml"]));

    flag ["ocaml"; "link"; "library"; "native"] (S[A"-cclib"; A"-Llib";
                                                   A"-cclib"; A"-lre2_stubs";
                                                   A"-cclib"; A"-lstdc++"]);
    flag ["ocaml"; "link"; "library"; "byte"]   (S[A"-dllib"; A"dllre2_stubs.so";
                                                   A"-cclib"; A"-Llib";
                                                   A"-cclib"; A"-lre2_stubs";
                                                   A"-cclib"; A"-lstdc++"]);
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook);;
