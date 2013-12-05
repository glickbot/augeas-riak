

module Test_Riak =

let lns = Riak.lns

test lns get "
% comment
[ foo
].
%comment 
" = { "#comment" = "comment" }
  {
    { "1" = "foo" }
  }
  { "#comment" = "comment" }

test lns get "[
{ foo, [ {bar, 1} ] }
]." = {
    { "foo"
      { "bar" = "1" }
    }
  }

test lns get "[ %comment1
{ foo, [ %comment2
    { bar, 1 }, %comment3
    { baz, 2 } %comment4
    ] %comment5
} % comment6
% comment7
].
%comment9
%comment10
" =  {
    { "#comment" = "comment1" }
    { "foo"
      { "#comment" = "comment2" }
      { "bar" = "1" }
      { "#comment" = "comment3" }
      { "baz" = "2"
        { "#comment" = "comment4" }
      }
      { "#comment" = "comment5" }
      { "#comment" = "comment6" }
      { "#comment" = "comment7" }
    }
  }
  { "#comment" = "comment9" }
  { "#comment" = "comment10" }

