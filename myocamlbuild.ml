(* OASIS_START *)
(* OASIS_STOP *)

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

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

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)

