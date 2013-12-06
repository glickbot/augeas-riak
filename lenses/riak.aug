module Riak =

autoload xfm

(*
let comment_generic = Util.comment_generic
let del_ws = Util.del_ws " "
let eol = Util.eol
let del_str = Util.del_str
let comment = comment_generic /%[ \t]*/ "% "
*)

(*

let del_ws = del /[ \t]+/ " "
let eol = del /\n/ "\n"
let comment = del /%+[ \t]*/ "% " . 
            [ label "#comment" . store /([^ \t\r\n%].*[^ \t\r\n]|[^ \t\r\n])\n/ ]

let opt_eol = (del_ws | eol)*

*)

let opt_eol = del /([ \t]+)|(%[^\n])|(\n)/ " "

let delim (s:string) = del s s . opt_eol

let comma = delim ","
let lbrace = delim "{"
let rbrace = delim "}"
let lbrack = delim "["
let rbrack = delim "]"
let lbin = delim "<<\""
let rbin = delim "\">>"
let end_stmt = delim "."

let param_regex = /([a-z0-9-][a-zA-Z0-9_.#-]*)|("([^"\\]*(\\.[^"\\]*)*)")|('([^'\\]*(\\.[^'\\]*)*)')/

let param_regex_key = /([a-z0-9-][a-zA-Z0-9_.#-]*)|"[^"\/\\]*"|'[^'\/\\]*'/

let param_key = key param_regex_key . opt_eol
let param_store = store param_regex . opt_eol
let param_path = store (param_regex - param_regex_key) . opt_eol

let param = param_store

let seq_items (s:string) (item:lens) = counter s .
                                     [ seq s . item ] . 
                                     ( comma . [ seq s . item ] )*

let seq_param = seq_items "param" param

let single_item (s:string) (item:lens) = counter s .
                                       [ seq s . item ]

let prop (block:lens) (tuple:lens) =
    let item = (block | param) in
                            [ lbrace
                            . param_key
                            . comma
                            . ( param                        (* {foo, bar} *)
                              | ( lbrack . block             (* {foo, [x]} *)
                                . ( comma . block )*
                                . rbrack )
                              | ( lbrack . seq_param . rbrack )
                              | single_item "prop" tuple     (* {foo, {} *)
                              | ( [ seq "item" . item ]      (* {foo, bar, baz*)
                                . ( comma 
                                  . [ seq "item" . item ] )+ (* 3+ items *)
                                )
                              )
                            . rbrace ]

let list (block:lens) =
    let item = (block | param) in
                     lbrack
                     . (seq_items "list" item)?
                     . rbrack

let single_tuple (block:lens) =
    let item = (block | param) in
                       lbrace
                       . (single_item "tuple" item)?
                       . rbrace

let non_param_tuple (block:lens) =
    let item = (block | param) in
                        lbrace
                        . counter "tuple"
                        . [ seq "tuple" . ( block | param_path ) ]
                        . ( comma . [ seq "tuple" . item ])+
                        . rbrace

let stuple (items:lens) = ( single_tuple items
                          | non_param_tuple items )

let lists (items:lens) = list items



let dummy = [ label "ETOODEEP" . store /ETOODEEP/ ]

let stuple_lvl5 = stuple dummy

let list_lvl5 = list dummy

let prop_lvl5 = prop dummy dummy

let stuple_lvl4 = stuple ( prop_lvl5 | stuple_lvl5 | list_lvl5 )

let list_lvl4 = list ( prop_lvl5 | stuple_lvl5 | list_lvl5 )

let prop_lvl4 = prop ( prop_lvl5
                     | list_lvl5
                     | stuple_lvl5 ) ( prop_lvl5
                                     | stuple_lvl5 )

let stuple_lvl3 = stuple ( prop_lvl4 | stuple_lvl4 | list_lvl4 )

let list_lvl3 = list ( prop_lvl4 | stuple_lvl4 | list_lvl4 )

let prop_lvl3 = prop ( prop_lvl4
                     | list_lvl4
                     | stuple_lvl4 ) ( prop_lvl4
                                     | stuple_lvl4 )

let stuple_lvl2 = stuple ( prop_lvl3 | stuple_lvl3 | list_lvl3 )

let list_lvl2 = list ( prop_lvl3 | stuple_lvl3 | list_lvl3 )

let prop_lvl2 = prop ( prop_lvl3
                     | list_lvl3
                     | stuple_lvl3 ) ( prop_lvl3
                                     | stuple_lvl3 )

let stuple_lvl1 = stuple ( prop_lvl2 | stuple_lvl2 | list_lvl2 )

let list_lvl1 = list ( prop_lvl2 | stuple_lvl2 | list_lvl2 )

let prop_lvl1 = prop ( prop_lvl2 
                     | list_lvl2 
                     | stuple_lvl2 ) ( prop_lvl2
                                     | stuple_lvl2 )

let app = prop ( prop_lvl1 
               | list_lvl1 
               | stuple_lvl1 ) ( prop_lvl1 
                               | stuple_lvl1 )


let lns = opt_eol
        . lbrack
        . app
        . (comma . app)*
        . rbrack
        . end_stmt


(*
let rec tuples = ( stuple ( lists tuples
                           | stuple tuples
                           | prop ( tuples|lists tuples ) tuples )
                 | prop ( tuples | lists tuples ) tuples )


let rec blocks = ( lists blocks | tuples )

let lns = opt_eol
        . lbrack
        . prop blocks tuples
        . ( comma . prop blocks tuples )*
        . rbrack
        . end_stmt

*)

let filter = incl "/etc/riak/app.config"

let xfm = transform lns filter
