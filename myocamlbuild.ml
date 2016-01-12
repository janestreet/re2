(* OASIS_START *)
(* OASIS_STOP *)

(* Temporary hacks *)
let js_hacks = function
  | After_rules ->
    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
            ]));

    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])
  | _ -> ()

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
    js_hacks hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)

