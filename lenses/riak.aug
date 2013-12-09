module Riak =
    autoload xfm
    
    let ws = del /([ \t]*((%.*)?\n)?)*/ ""

    let atom_rx = /[a-z][a-zA-Z0-9_]*/
    let number_rx = /-?[0-9]+((\.[0-9]+(e[0-9]+)?)|(#[0-9a-zA-Z]+))?/
    (* let char_rx = /$[\\]?[^ \t\n%]/ *)
    let quoted_rx = /("([^"\\]*(\\.[^"\\]*)*)")|('([^'\\]*(\\.[^'\\]*)*)')/
    let binary_rx = /<<(([^"'<>]*)|("([^"\\]*(\\.[^"\\]*)*)"))*>>/

    let delim (s:string) = del ( s . /([ \t]*((%.*)?\n)?)*/ ) s


    let comma = delim ","
    let lbrace = delim "{"
    let rbrace = delim "}"
    let lbrack = delim "["
    let rbrack = delim "]"
    let end_stmt = delim "."

    let binary = [ label "#bin"
                 . del /<</ "<<"
                 . store binary_rx
                 . del />>/ ">>" . ws ]

    let quoted = [ label "#str"
                 . del /"/ "\""
                 . store /([^"\\]*(\\.[^"\\]*)*)/
                 . del /"/ "\"" . ws ]
    
    let squoted = [ label "#qatom"
                 . del /'/ "'"
                 . store /([^'\\]*(\\.[^'\\]*)*)/
                 . del /'/ "'" . ws ]

    let atom = [ label "#atom" . store atom_rx . ws ]
    let number = [ label "#num" . store number_rx . ws ]
    (* let char = [ label "char" . store char_rx . ws ] *)

    let any_param = (atom|number|quoted|squoted)
    let not_atom = (number|quoted|squoted)

    let prop (next:lens) = [ label "#prop"
                           . store atom_rx . ws
                           . comma . any_param  ]

    let proplist (next:lens) = [ key atom_rx . ws . comma
                               . lbrack
                               . ( next . ( comma . next )* )?
                               . rbrack ]

    let onetuple (next:lens) = [ label "#single_tuple" . (next|any_param) ]

    let twotuple (next:lens) =  [ label "#double_tuple" . (next|not_atom)
                   . comma . (next|any_param) ]

    let tuple (next:lens) = [ label "#tuple" . (next|any_param)
                . comma . (next|any_param)
                . ( comma . (next|any_param) )+ ]

    let proptuple (next:lens) = [ label "#prop_tuple"
                                . store atom_rx . ws
                                . comma
                                . lbrace
                                . ( prop next
                                  | proplist next
                                  | onetuple next
                                  | twotuple next
                                  | tuple next )?
                                . rbrace ]

    let list (next:lens) =  [ label "#list"
                . ( (next|any_param)
                . ( comma . (next|any_param) )* )? ]


    let element (next:lens) =
        ( ( lbrace . ( prop next
                 | proplist next
                 | proptuple next
                 | onetuple next
                 | twotuple next
                 | tuple next )
          . rbrace )
        | ( lbrack . ( list next ) . rbrack ) )

    let rec elements = element elements

    let lns = ws
            . lbrack
            . ( elements . ( comma . elements )* )
            . rbrack
            . end_stmt

    let filter = incl "/etc/riak/app.config"

    let xfm = transform lns filter
