let package_name = "re2"

let sections =
  [ ("lib",
    [ ("built_lib_re2", None)
    ],
    [ ("META", None)
    ; ("_build/lib/libre2_stubs.a", None)
    ])
  ; ("stublibs",
    [],
    [ ("_build/lib/dllre2_stubs.so", None)
    ])
  ]
