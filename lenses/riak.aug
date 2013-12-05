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

let  test_block = ( opt_eol | blocks )

test test_block get "{ foo, bar }" = { "foo" = "bar" }
test test_block get "{ foo }" = { "1" = "foo" }
test test_block get "{{foo}, [ foo ]}" = { "1"
    { "2" = "foo" }
  }
  { "1"
    { "1" = "foo" }
  }
test test_block get "{ foo, [ bar ] }" = { "foo"
    { "1" = "bar" }
  }

test test_block get "{anti_entropy, {on, []}}" = { "anti_entropy"
    { "1"
      { "on" }
    }
  }

test test_block get "{ foo, [ { bar, baz } ] }" = { "foo"
    { "bar" = "baz" }
  }
test test_block get "{ foo }" = { "1" = "foo" }
test test_block get "{ { hi } }" = { "1"
    { "1" = "hi" }
  }
test test_block get "{ {boo}, param }" = { "1"
    { "2" = "boo" }
  }
  { "1" = "param" }
test test_block get "{ foo, [ { foo, bar }, { bar, baz } ] }" = { "foo"
    { "foo" = "bar" }
    { "bar" = "baz" }
  }
test test_block get "{ foo, bar, baz }" = { "foo"
    { "2" = "bar" }
    { "1" = "baz" }
  }
test test_block get "{{hi}}" = { "1"
    { "1" = "hi" }
  }
test test_block get "{{hi},{hi}}" = { "1"
    { "2" = "hi" }
  }
  { "1"
    { "1" = "hi" }
  }
test test_block get "{ foo, {foo, bar, baz}}" = 
{ "foo"
    { "1"
      { "foo"
        { "2" = "bar" }
        { "1" = "baz" }
      }
    }
  }
test test_block get "{ foo, {foo, bar}, { baz, bam }}" =
  { "foo"
    { "2"
      { "foo" = "bar" }
    }
    { "1"
      { "baz" = "bam" }
    }
  }

test test_block get "%comment
" = { "#comment" = "comment" }

test test_block get "{foo, \"bar\"}" = { "foo" = "\"bar\"" }
test test_block get "{foo, bar}" = { "foo" = "bar" }
test test_block get "{foo, 10}" = { "foo" = "10" }

test test_block get "[ atom ]" = { "1" = "atom" }

test test_block get "[ atom, \"10.0.0.1\" ]" =
    { "2" = "atom" }
    { "1" = "\"10.0.0.1\"" }

test test_block get "[ 
atom,
123
]" =
    { "2" = "atom" }
    { "1" = "123" }

test test_block get "[ %something
atom %something else
]
% a final thing
" = 
    { "#comment" = "something" }
    { "1" = "atom"
      { "#comment" = "something else" }
    }
    { "#comment" = "a final thing" }

test test_block get "[ {foo,bar}, {baz,bam} ]" =
  { "2"
    { "foo" = "bar" }
  }
  { "1"
    { "baz" = "bam" }
  }

let hi_5 = { "1" { "hi" = "5" } }

test test_block get "[{hi,5}]" = hi_5
test test_block get "[ {hi,5} ] " = hi_5
test test_block get "[ { hi, 5} ] " = hi_5
test test_block get "[ {hi ,5 } ] " = hi_5

test test_block get "{ \"123\", foo, bar }" =
  { "\"123\""
    { "2" = "foo" }
    { "1" = "bar" }
  }

test test_block get "{ \"127.0.0.1\", 80 }" =
  { "\"127.0.0.1\"" = "80" }

test test_block get "[ {\"127.0.0.1\", 80}, {foo, bar} ]" =
  { "2"
    { "\"127.0.0.1\"" = "80" }
  }
  { "1"
    { "foo" = "bar" }
  }

test test_block get "{foo, [ {bar, baz} ]}" = 
  {"foo"
      { "bar" = "baz" }
  }

test test_block get "{foo, [ {bar, [{\"127.0.0.1\", 80}]}]}" =
  { "foo"
    { "bar"
      { "\"127.0.0.1\"" = "80" }
    }
  }

test lns get "[ { foo, bar } ]." =
    { "foo" = "bar" }

test lns get "[ { foo, [ { bar, baz } ] } ]." = 
{ "foo"
    { "bar" = "baz" }
}

test lns get "[
{ proplist1, [
    { opt1, 1 },
    { opt2, \"2\" },
    { opt3, three }
]},
{ proplist2, [
    { opt1, 22 },
    { opt2, \"23\" },
    { opt3, twentyfour }
]}
]." = { "proplist1"
    { "opt1" = "1" }
    { "opt2" = "\"2\"" }
    { "opt3" = "three" }
  }
  { "proplist2"
    { "opt1" = "22" }
    { "opt2" = "\"23\"" }
    { "opt3" = "twentyfour" }
  }

test test_block get "{handlers, [
    {lager_file_backend, [
        {\"/var/log/riak/error.log\", error, 10485760, \"\", 5},
        {\"/var/log/riak/console.log\", info, 10485760, \"\", 5}
        ]}
    ]}" = { "handlers"
    { "lager_file_backend"
      { "5" = "\"/var/log/riak/error.log\"" }
      { "4" = "error" }
      { "3" = "10485760" }
      { "2" = "\"\"" }
      { "1" = "5" }
      { "5" = "\"/var/log/riak/console.log\"" }
      { "4" = "info" }
      { "3" = "10485760" }
      { "2" = "\"\"" }
      { "1" = "5" }
    }
  }

(*
test lns get "
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
[
 %% Riak Client APIs config
 {riak_api, [
            %% pb_backlog is the maximum length to which the queue of pending
            %% connections may grow. If set, it must be an integer >= 0.
            %% By default the value is 5. If you anticipate a huge number of
            %% connections being initialised *simultaneously*, set this number
            %% higher.
            %% {pb_backlog, 64},

            %% pb is a list of IP addresses and TCP ports that the Riak
            %% Protocol Buffers interface will bind.
            {pb, [ {\"127.0.0.1\", 8087 } ]}
            ]},
{lager, [
            %% What handlers to install with what arguments
            %% The defaults for the logfiles are to rotate the files when
            %% they reach 10Mb or at midnight, whichever comes first, and keep
            %% the last 5 rotations. See the lager README for a description of
            %% the time rotation format:
            %% https://github.com/basho/lager/blob/master/README.org
            %%
            %% If you wish to disable rotation, you can either set the size to 0
            %% and the rotation time to \"\", or instead specify a 2-tuple that only
            %% consists of {Logfile, Level}.
            %%
            %% If you wish to have riak log messages to syslog, you can use a handler
            %% like this:
            %%   {lager_syslog_backend, [\"riak\", daemon, info]},
            %%
            {handlers, [                            {lager_file_backend, [                                {\"/var/log/riak/error.log\", error, 10485760, \"\", 5},                                {\"/var/log/riak/console.log\", info, 10485760, \"\", 5}                            ]}                        ] },

            %% Whether to write a crash log, and where.
            %% Commented/omitted/undefined means no crash logger.
            {crash_log, \"/var/log/riak/crash.log\"},

            %% Maximum size in bytes of events in the crash log - defaults to 65536
            {crash_log_msg_size, 65536},

            %% Maximum size of the crash log in bytes, before its rotated, set
            %% to 0 to disable rotation - default is 0
            {crash_log_size, 10485760},

            %% What time to rotate the crash log - default is no time
            %% rotation. See the lager README for a description of this format:
            %% https://github.com/basho/lager/blob/master/README.org
            {crash_log_date, \"\"},

            %% Number of rotated crash logs to keep, 0 means keep only the
            %% current one - default is 0
            {crash_log_count, 5},

            %% Whether to redirect error_logger messages into lager - defaults to true
            {error_logger_redirect, true}
        ]}
]." = ?
*)
