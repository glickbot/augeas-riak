
(* Currently only passes with augparse --notypecheck *)

module Test_Riak =

let test_block = (Riak.opt_eol | Riak.blocks)
let lns = Riak.lns

let test_prop = (Riak.prop Riak.dummy Riak.dummy)

test test_prop get "{ foo, bar }" = ?

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

