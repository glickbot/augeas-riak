(*
Module: Riak
  Parses Riak 1.x erlang based app.config files:
  riak/app.config
  riak-cs/app.config
  stanchion/app.config

Author: Jon Glick <jglick@basho.com>

About: Reference
    This lens tries to parse erlang "proplists" akin to this:
    <http://www.erlang.org/doc/man/proplists.html>

    Also tries to be as true to erlang data types as is necessary:
    <http://www.erlang.org/doc/reference_manual/data_types.html>

About: Licence
  This file is licensed under the LGPLv2+, like the rest of Augeas.
*)

module Riak =
    autoload xfm

(************************************************************************
 * Group:                 DATATYPE REGEXS
 ************************************************************************)

    
    let ws = del /([ \t]*((%.*)?\n)?)*/ ""

    let atom_rx = /[a-z][a-zA-Z0-9_]*/
    let number_rx = /-?[0-9]+((\.[0-9]+(e[0-9]+)?)|(#[0-9a-zA-Z]+))?/
    (* let char_rx = /$[\\]?[^ \t\n%]/ *)
    let quoted_rx = /("([^"\\]*(\\.[^"\\]*)*)")|('([^'\\]*(\\.[^'\\]*)*)')/
    let binary_rx = /<<(([^"'<>]*)|("([^"\\]*(\\.[^"\\]*)*)"))*>>/


(************************************************************************
 * Group:                 USEFUL PRIMITIVES
 ************************************************************************)


(* View: delimiters
    various delimiters which can have any amount of whitespace/comments after *)

    let delim (s:string) = del s s . ws

    let comma = delim ","
    let lbrace = delim "{"
    let rbrace = delim "}"
    let lbrack = delim "["
    let rbrack = delim "]"
    let end_stmt = delim "."

(************************************************************************
 * Group:                 DATATYPES
 ************************************************************************)

(* View: binary
    <http://www.erlang.org/doc/reference_manual/data_types.html#id73739>
    Erlang binary terms (very inclusive) *)
    let binary = [ label "#bin"
                 . del /<</ "<<"
                 . store binary_rx
                 . del />>/ ">>" . ws ]


(* View: quoted
    <http://www.erlang.org/doc/reference_manual/data_types.html#id67689>
    Erlang "strings" *)
    let quoted = [ label "#str"
                 . del /"/ "\""
                 . store /([^"\\]*(\\.[^"\\]*)*)/
                 . del /"/ "\"" . ws ]

(* View: squoted
    <http://www.erlang.org/doc/reference_manual/data_types.html#id66442>
    Erlang single-quoted atoms *)
    let squoted = [ label "#qatom"
                 . del /'/ "'"
                 . store /([^'\\]*(\\.[^'\\]*)*)/
                 . del /'/ "'" . ws ]

(* View: atom
    <http://www.erlang.org/doc/reference_manual/data_types.html#id66442>
    Erlang non-quoted atom *)
    let atom = [ label "#atom" . store atom_rx . ws ]
(* View: number
    <http://www.erlang.org/doc/reference_manual/data_types.html#id62516>
    Erlang number *)
    let number = [ label "#num" . store number_rx . ws ]

(* View: char
    <http://www.erlang.org/doc/reference_manual/data_types.html#id62516>
    Erlang shorthand for ASCII characters
    Currently unsupported, use plain integers instead *)
    (* let char = [ label "char" . store char_rx . ws ] *)

(* View: any_param
    Any data type ( atom, number quoted, squoted ) *)
    let any_param = (atom|number|quoted|squoted)

(* View: non_atom
    Any data type *except* atoms
    Used for proplist naming/labels *)
    let not_atom = (number|quoted|squoted)

(************************************************************************
 * Group:                 ERLANG SECTIONS
 ************************************************************************)

(* View: prop
    Simple property from a tuple of 2 terms with the first being an atom
    { option1, value1 } => { "#prop" = "option1" { "#atom" = "value" } } *)
    let prop (next:lens) = [ label "#prop"
                           . store atom_rx . ws
                           . comma . any_param  ]

(* View: proplist
    A tuple of 2 terms, the first being an atom, the second a list.
    The atom is the label, the list isn't a seperate child, allowing:
    {foo, [{bar, [{bam,baz}]}]} => {"foo", {"bar", {"#prop" = ... } } } *)
    let proplist (next:lens) = [ key atom_rx . ws . comma
                               . ( lbrack
                                   . next . ( comma . next )*
                                   . rbrack
                                 | lbrack . rbrack ) ]

(* View: onetuple
    Handles the cases of a single, allows anything *)
    let onetuple (next:lens) = [ label "#single_tuple" . (next|any_param) ]

(* View: twotuple
    Handles the case of what would otherwise be a prop type, but the
    first term *is not* an atom ( thus the not_atom lens ) *)
    let twotuple (next:lens) =  [ label "#double_tuple" . (next|not_atom)
                   . comma . (next|any_param) ]

(* View: tuple
    Handles the case of any tuple with 3 or more values *)
    let tuple (next:lens) = [ label "#tuple" . (next|any_param)
                . comma . (next|any_param)
                . ( comma . (next|any_param) )+ ]
(* View: proptuple
    Handles the properties (i.e. {name, value}, where value is a tuple
    ( i.e. {foo, {bar, baz}} instead of {foo, [{bar,baz}]} *)
    let proptuple (next:lens) = [ label "#prop_tuple"
                                . store atom_rx . ws
                                . comma
                                . ( lbrace
                                  . ( prop next
                                    | proplist next
                                    | onetuple next
                                    | twotuple next
                                    | tuple next )
                                  . rbrace
                                  | lbrace . rbrace ) ]

(* View: element
    Handles the enclosing delimiters for the sections, and recursion *)
    let element (next:lens) =
        ( ( lbrace . ( prop next
                 | proplist next
                 | proptuple next
                 | onetuple next
                 | twotuple next
                 | tuple next )
          . rbrace )
        | ( [ label "#list"
            . ( ( lbrack . (next|any_param)
                . ( comma . (next|any_param) )*
                . rbrack )
              | ( lbrack . rbrack ) ) ] ) )

(* View: elements
    The recusive element *)
    let rec elements = element elements

(************************************************************************
 * Group:                     LENS
 ************************************************************************)

    let lns = ws
            . lbrack
            . ( elements . ( comma . elements )* )
            . rbrack
            . end_stmt

    let filter = incl "/etc/riak/app.config"
               . incl "/etc/riak-cs/app.config"
               . incl "/etc/stanchion/app.config"

    let xfm = transform lns filter
