open Core

module type S = sig
  (** A value of type ['a t] is a regex that parses ['a]s.
      The matching is implemented using Re2.

      UTF-8 is supported by Re2 but not by this module. This is because we want to use
      [char] as a character type, but that's just wrong in a multibyte encoding. *)
  type 'a t [@@deriving sexp_of]

  include Regex_parser_intf.S with type 'a t := 'a t

  (** [to_regex_string] and [to_re2] both forget what a ['a t] knows
      about turning the matching strings into ['a]s *)
  val to_regex_string : _ t -> string

  val to_re2 : ?case_sensitive:bool -> _ t -> Regex.t

  (** [of_re2 r] forgets the options that [r] was compiled with, instead using
      [`Encoding_latin1 true], [`Dot_nl true], and the case-sensitivity setting of the
      overall pattern. You can still try and use '(?flags:re)' Re2 syntax to set options
      for the scope of this regex.

      The returned values are precisely the captures of the underlying regex, in order:
      note that unlike (say) [Re2.Match.get_all], the whole match is *not* included (if
      you want that, just use [capture]). Named captures are not accessible by name. *)
  val of_re2 : Regex.t -> string option array t
end

module type Parser = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S = S with type 'a t = 'a t
  end

  include
    Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  include Open_on_rhs_intf.S with type 'a t := 'a t
end
