module Test_Riak = 

    let test_element = Riak.elements

    let lns = Riak.lns

    let ws = Riak.ws
    let lbrace = Riak.lbrace
    let rbrace = Riak.rbrace
    let lbrack = Riak.lbrack
    let rbrack = Riak.rbrack
    
    let any_param = Riak.any_param

    let prop = Riak.prop
    let proptuple = Riak.proptuple
    let proplist = Riak.proplist
    let onetuple = Riak.onetuple
    let twotuple = Riak.twotuple
    let list = Riak.list

    let dummy = [ label "dummy" . store /\./ . ws ]

    test any_param get "atom" = { "#atom" = "atom" }
    test any_param get "\"quoted\"" = { "#str" = "quoted" }
    test any_param get "'squoted'" = { "#qatom" = "squoted" }
    test any_param get "5#16" = { "#num" = "5#16" }
    (*test any_param get "$\\n" = { "char" = "$\\n" }*)

    let start = "[ { foo, [ {bar, baz} ]} ]."

    test lns get start = { "foo"
    { "#prop" = "bar"
      { "#atom" = "baz" }
    }
  }

    test lns put start after
        set "/foo/#prop" "bam" = "[ { foo, [ {bam, baz} ]} ]."

    test lns put start after
        set "/foo/#prop[1]/#atom" "boom" = "[ { foo, [ {bar, boom} ]} ]."

    (* ---- prop tests ---- *)

    let test_prop = lbrace . prop dummy . rbrace
    test test_prop get "{foo, bar}" = { "#prop" = "foo"
    { "#atom" = "bar" }
  }

    (* ---- proptupe tests ---- *)

    let test_proptuple = lbrace . proptuple dummy . rbrace

    test test_proptuple get "{foo, {foo}}" = 
  { "#prop_tuple" = "foo"
    { "#atom" = "foo" }
  }

    let test_proptuple2 = lbrace . proptuple (test_prop|test_proptuple) . rbrace
    
    test test_proptuple2 get "{foo, {foo}}" =  { "#prop_tuple" = "foo"
    { "#atom" = "foo" }
  }

    (* ---- proplist tests ---- *)

    let test_proplist = lbrace . proplist test_prop . rbrace

    test test_proplist get "{foo, [ {bar, baz}, {bam, baf} ]}" =
  { "foo"
    { "#prop" = "bar"
      { "#atom" = "baz" }
    }
    { "#prop" = "bam"
      { "#atom" = "baf" }
    }
  }
    
    test test_proplist get "{foo, [ {bar, \"eighty two\"}, {bam, 38}]}" =  { "foo"
    { "#prop" = "bar"
      { "#str" = "eighty two" }
    }
    { "#prop" = "bam"
      { "#num" = "38" }
    }
  }
    
   let test_proplist0 = lbrace . proplist test_proplist . rbrace
   test test_proplist0 get "{foo, [{foo, [ {bar, \"eighty two\"}, {bam, 38}]}]}" = { "foo"
    { "foo"
      { "#prop" = "bar"
        { "#str" = "eighty two" }
      }
      { "#prop" = "bam"
        { "#num" = "38" }
      }
    }
  }


   (* --- multi-prop tests ---- *)

   let test_props = (test_prop|test_proplist)

   test test_props get "{foo, bar}" = { "#prop" = "foo"
    { "#atom" = "bar" }
  }

   let test_props1 = (test_prop|test_proptuple)

   test test_props1 get "{foo, bar}" = { "#prop" = "foo"
    { "#atom" = "bar" }
  }

   let test_props3 = (test_proplist|test_proptuple)

   test test_props3 get "{foo, [{bar, baz}]}" =  { "foo"
    { "#prop" = "bar"
      { "#atom" = "baz" }
    }
  }

   let test_any_prop = (test_prop|test_proplist|test_proptuple)

   test test_any_prop get "{foo, bar}" = { "#prop" = "foo"
    { "#atom" = "bar" }
  }

   (* ---- onetuple tests ---- *)
    
    let test_onetuple = lbrace . onetuple dummy . rbrace

    test test_onetuple get "{bar}" = { "#single_tuple"
    { "#atom" = "bar" }
    }

    test test_onetuple get "{'quoted'}" = { "#single_tuple" { "#qatom" = "quoted"}}

    (* ---- twotuple (onetuple) tests ---- *)

    let test_twotuple = lbrace . twotuple test_onetuple . rbrace

    test test_twotuple get "{'foo', bar}" = { "#double_tuple"
    { "#qatom" = "foo" }
    { "#atom" = "bar" }
  }
    test test_twotuple get "{5, bar}" = {
         "#double_tuple" {"#num" = "5"}{"#atom"="bar"}
         }

    test test_twotuple get "{{atom}, bar}" = { "#double_tuple"
    { "#single_tuple"
      { "#atom" = "atom" }
    }
    { "#atom" = "bar" }
  }

    test test_twotuple get "{{atom},{atom}}" = { "#double_tuple"
    { "#single_tuple"
      { "#atom" = "atom" }
    }
    { "#single_tuple"
      { "#atom" = "atom" }
    }
  }

  (* ---- test list (onetuple|twotuple) ---- *)

    let test_list = lbrack . list (test_onetuple|test_twotuple) . rbrack

    test test_list get "[ a, b, c ]" = { "#list"
    { "#atom" = "a" }
    { "#atom" = "b" }
    { "#atom" = "c" }
  }

    test test_list get "[ { a }, { b }, { c } ]" = { "#list"
    { "#single_tuple"
      { "#atom" = "a" }
    }
    { "#single_tuple"
      { "#atom" = "b" }
    }
    { "#single_tuple"
      { "#atom" = "c" }
    }
  }
  
    test test_list get "[ {'a',b}, {5, c} ]" = { "#list"
    { "#double_tuple"
      { "#qatom" = "a" }
      { "#atom" = "b" }
    }
    { "#double_tuple"
      { "#num" = "5" }
      { "#atom" = "c" }
    }
  }
    
    test test_list get "[ {a}, {4, b} ]" = { "#list"
    { "#single_tuple"
      { "#atom" = "a" }
    }
    { "#double_tuple"
      { "#num" = "4" }
      { "#atom" = "b" }
    }
  }
 
    test test_list get "[ {one}, {{two}, three}, {'four',{five}} ]" = { "#list"
    { "#single_tuple"
      { "#atom" = "one" }
    }
    { "#double_tuple"
      { "#single_tuple"
        { "#atom" = "two" }
      }
      { "#atom" = "three" }
    }
    { "#double_tuple"
      { "#qatom" = "four" }
      { "#single_tuple"
        { "#atom" = "five" }
      }
    }
  }


(*

    let element (next:lens) = 
    (( lbrace 
    . ( [ key atom_rx . ws . comma . any_param ]
      | [ key atom_rx . ws . comma
           . lbrack . ( next . ( comma . next )* ) . rbrack ]
      | [ label "#1_tuple" . (next|any_param) ] *)
 (*     | [ label "#2_tuple" . (next|not_atom) . comma . (next|any_param) ] *)
 (*     | [ label "#tuple" . (next|any_param) . comma . (next|any_param)
          . ( comma . (next|any_param) )+ ] *)
(*      )
    . rbrace ) *)
 (*   |( lbrack
     . [ label "#list"
       . ( (next|any_param)
         . ( comma . (next|any_param) )* )? ]
     . rbrack )) *)

(*

    let tuple (l:lens) (t:lens) (p:lens) = [
        label "#tuple"
        . lbrace
        . ( ( l|t|p|path|binary ) . comma
          . (Build.opt_list (l|t|p|any_param) comma)? )?
        . rbrace ]

    let stuple (

    let list (l:lens) (t:lens) = [
        label "#list"
        . lbrack
        . (Build.opt_list (l|t|any_param) comma)?
        . rbrack ]



    let prop (tuple (p:lens) = [
        lbrace
        . key param_regex . comma
        . ( comma
          . ( any_param
            | ( lbrack
              . ( p . ( p . comma )* )?
              . rbrack ) )?
          )?
        . rbrace ]


    let rec r_prop = prop r_prop

    let rec 

    let element (l:lens) (t:lens) (p:lens) =

        (list l t p|tuple l t p|prop l t p)

    let rec elements = element 



    let element (elements:lens) =

        let tuple = [ label "#tuple" 
                    . lbrace
                    . ( (elements|param_path|binary) . comma
                        . (Build.opt_list (elements|any_param) comma)?
                        )?
                    . rbrace ] in

        let prop = [ lbrace
                   . key param_regex . comma
                   . ( any_param
                   | ( lbrack
                     . 

                   . ( store any_regex . ws | elements )
                   . rbrace ] in

        

        
        let list = [ label "#list"
                   . lbrack
                   . (Build.opt_list (elements|any_param) comma)?
                   . rbrack ] in

        
        ( tuple | list | prop )
*)

(*                   
    let dummy = [ label "dummy" . store /\./ . ws ]

    let element0 = element dummy
    let element1 = element element0
    let element2 = element element1
    let element3 = element element2

*)
    
(*
    let rec elements = element elements
*)
(*
    test element3 get "{ foo, bar }" = ?
    let lns = ws
            . lbrack
            . ( element3
              . ( comma . element3 )* )
            . rbrack
            . end_stmt
*)
(*
    let lns = ws
            . lbrack 
            . ( elements
            . ( comma . elements )* )
            . rbrack
            . end_stmt
*)
(*
    let filter = incl "/etc/riak/app.config"

    test lns get "[ {} ]." = { "#tuple" }
    test lns get "[ [] ]." = { "#list" }

    test lns get "[ { \"/path/to/location\", bar } ]." = { "#tuple"
    { "path" = "\"/path/to/location\"" }
    { "value" = "bar" }
    }

    test lns get "[ { {}, baz } ]." = { "#tuple"
    { "#tuple" }
    { "value" = "baz" }
    }

    test lns get "[ {foo, bar} ]." = { "foo" = "bar" }

    let xfm = transform lns filter


    let start = "[ { foo, [ { bar, baz } ] } ]."
    test lns get start = { "foo"
    { "#list"
      { "bar" = "baz" }
    }
    }
    *)
    (*{ "config"
    { "#tuple"
      { "value" = "foo" }
      { "#list"
        { "value" = "bar" }
        { "value" = "baz" }
      }
    }
    }
*)
(*
    let blank = "[ {foo, bar} ]."

    test lns get blank = { "foo" = "bar" }

    test lns put blank after
        set "/foo" "bam" = "[ {foo, bam} ]."

    test lns put blank after
        insa "bar" "/";
        set "/bar" "boom" = "[ {foo, bar} ,{bar,boom}]."

    let test2 = "[ { alpha, [{a, apples}] }, { beta, [{b, bananas}] } ]."
    
    test lns get test2 = ?
*)
(*    
    test lns put test2 after
        set "/alpha/#list/a" "ants" = ?
*)


(*

    let sample1 = "[
 {riak_api, [
            {pb, [ {\"127.0.0.1\", 8087 } ]}
            ]},
 {riak_core, [
              {ring_state_dir, \"./data/ring\"},
              {http, [ {\"127.0.0.1\", 8098 } ]},
              {handoff_port, 8099 },
              {dtrace_support, false},
              {platform_bin_dir, \"./bin\"},
              {platform_data_dir, \"./data\"},
              {platform_etc_dir, \"./etc\"},
              {platform_lib_dir, \"./lib\"},
              {platform_log_dir, \"./log\"}
             ]}
]."

    test lns get sample1 =  { "#config"
    { "#tuple"
      { "value" = "riak_api" }
      { "#list"
        { "#tuple"
          { "value" = "pb" }
          { "#list"
            { "#tuple"
              { "value" = "\"127.0.0.1\"" }
              { "value" = "8087" }
            }
          }
        }
      }
    }
    { "#tuple"
      { "value" = "riak_core" }
      { "#list"
        { "#tuple"
          { "value" = "ring_state_dir" }
          { "value" = "\"./data/ring\"" }
        }
        { "#tuple"
          { "value" = "http" }
          { "#list"
            { "#tuple"
              { "value" = "\"127.0.0.1\"" }
              { "value" = "8098" }
            }
          }
        }
        { "#tuple"
          { "value" = "handoff_port" }
          { "value" = "8099" }
        }
        { "#tuple"
          { "value" = "dtrace_support" }
          { "value" = "false" }
        }
        { "#tuple"
          { "value" = "platform_bin_dir" }
          { "value" = "\"./bin\"" }
        }
        { "#tuple"
          { "value" = "platform_data_dir" }
          { "value" = "\"./data\"" }
        }
        { "#tuple"
          { "value" = "platform_etc_dir" }
          { "value" = "\"./etc\"" }
        }
        { "#tuple"
          { "value" = "platform_lib_dir" }
          { "value" = "\"./lib\"" }
        }
        { "#tuple"
          { "value" = "platform_log_dir" }
          { "value" = "\"./log\"" }
        }
      }
    }
  }
*)
    (*
    test param get "atom" = ?
    test param get "'atom'" = ?
    test param get "\"This is a quote\"\" = ?
    test param get "<<\"Silly binaries\">>" = ?
    test param get "1" = ?
    test param get "3.5" = ?
    test param get "8#16" = ?
    test param get "true" = ?
    let element0 = param
    let element1 = element element0
    let element2 = element element1
    let element3 = element element2

    let test_prop = element3

    test test_prop get "{foo,bar}" = ?
    test test_prop get "{ foo, bar}" = ?
    test test_prop get "{foo ,bar }" = ?
    test test_prop get "{ foo , bar } " = ?
    test test_prop get "{ foo ,
    bar } " = ?

    test test_prop get "{ foo, %something
    bar }" = ?

    test test_prop get "{ {foo}, [ bar, baz ]}" = ?
    *)

