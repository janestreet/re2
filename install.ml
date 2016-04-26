#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"re2"
  [ oasis_lib "re2"
  ; file "META" ~section:"lib"
  ; file "_build/lib/libre2_stubs.a" ~section:"lib"
  ; file "_build/lib/dllre2_stubs.so" ~section:"stublibs"
  ]
