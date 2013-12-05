module Riak =

autoload xfm

let comment_generic = Util.comment_generic
let del_ws = Util.del_ws " "
let eol = Util.eol
let del_str = Util.del_str

let comment = comment_generic /[ \t]*%[ \t]*/ "% "

let opt_eol = (del_ws | comment | eol )*

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
                              | ( lbrack . block?             (* {foo, []} *)
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

let stuples (items:lens) = ( single_tuple items
                          | non_param_tuple items )

let lists (items:lens) = list items

let rec tuples = ( stuples ( lists tuples
                           | stuples tuples
                           | prop ( tuples|lists tuples ) tuples )
                 | prop ( tuples | lists tuples ) tuples )


let rec blocks = ( lists blocks | tuples )

let lns = opt_eol
        . lbrack
        . prop blocks tuples
        . ( comma . prop blocks tuples )*
        . rbrack
        . end_stmt

let filter = incl "/etc/riak/app.config"

let xfm = transform lns filter
