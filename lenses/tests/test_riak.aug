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
        set "/foo/#prop" "bam" = "[ { foo, [ {bam, baz}]} ]."

    test lns put start after
        set "/foo/#prop[1]/#atom" "boom" = "[ { foo, [ {bar, boom}]} ]."

    (* ---- prop tests ---- *)

    let test_prop = lbrace . prop dummy . rbrace
    test test_prop get "{foo, bar}" = { "#prop" = "foo"
    { "#atom" = "bar" }
  }

    (* ---- proptupe tests ---- *)
(*
    let test_proptuple = lbrace . proptuple dummy . rbrace

    test test_proptuple get "{foo, {foo}}" = { "#prop_tuple" = "foo"
    { "#single_tuple"
      { "#atom" = "foo" }
    }
  }

    let test_proptuple2 = lbrace . proptuple (test_prop|test_proptuple) . rbrace
    
    test test_proptuple2 get "{foo, {foo}}" =  { "#prop_tuple" = "foo"
    { "#atom" = "foo" }
  }
*)
    (* ---- proplist tests ---- *)

    let test_proplist = lbrace . proplist test_prop . rbrace

    test test_proplist get "{foo, []}" = { "foo" }

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
(*
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
*)
   (* ---- onetuple tests ---- *)
    
    let test_onetuple = lbrace . onetuple test_proplist . rbrace

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


