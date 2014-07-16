## 111.08.00

- Upgraded to upstream library version 20140304.

    Added options `Dot_nl` and `Never_capture`.

## 111.06.00

- Added `Re2.Std`, so that one should now use `Re2` via `module Re2 =
  Re2.Std.Re2`.

    At some future date, we will rename the `Regex` module to
    `Re2_internal` to force the stragglers to update to the new
    convention.

## 111.03.00

- Fixed a bug with `replace_exn` and anchoring.

    Fixed this bug:

        $ R.replace_exn ~f:(fun _ -> "a") (R.create_exn "^") "XYZ";;
        - : string = "aXaYaZa"

        $ R.replace_exn ~f:(fun _ -> "a") (R.create_exn "^X") "XXXYXXZ";;
        - : string = "aaaYXXZ"

## 109.53.00

- Bump version number

## 109.52.00

- Fixed bugs in `Re2.Regexp.find_all` and `Re2.Regexp.find_first`.
    * memory leaks on errors
    * unlikely garbage in their return values, or even segfaults
      (especially unlikely for `find_first`)

## 109.32.00

- Fixed a bug in the C bindings that could cause a segfault.

    Fixed a bug where `mlre2__create_re` in C can give OCaml a freed C string.

    The bug was in:

    ```c
    if (!compiled->ok()) {
      compile_error = compiled->error().c_str();
      delete compiled;
      compiled = NULL;
      caml_raise_with_string(*caml_named_value("mlre2__Regex_compile_failed"),
          compile_error);
    }
    ```

    This is in `mlre2__create_re` if we fail to compile the regular
    expression.  Notice how we delete the re object before we use the pts
    to its' error string.  (Notice that in C++ `c_str()` returns a pointer
    to the internal data of the string object it does NOT create a copy
    and `error()` just returns a reference to the regular expression objects
    error string member `*error_`).

    So if `caml_raise_with_string` has to allocate on the heap to create
    the exception or the copy of the string that might invalidate the ptr
    before we will copy it.

