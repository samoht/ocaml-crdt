let count = ref 0

let test name contents string_of_x string_of_y tests =
  incr count;
  Printf.printf
    "=====================================\n\
    \  %d. %s\n\
     -------------------------------------\n"
    !count name;
  List.iter (fun (name, x, y) ->
      Printf.printf "%s: %s %s\n" name (string_of_x x) (string_of_y y);
      assert (contents x = y)
    ) tests;
  Printf.printf "\n%!"

let int = string_of_int

let list f l =
  let s = List.map f l in
  Printf.sprintf "{ %s }" (String.concat " " s)

module S = struct
  type t = string
  let compare = String.compare
  let to_string x = x
end

module Foo = struct
  include S
  let me = "foo"
end

module Bar = struct
  include S
  let me = "bar"
end

module ClockTest = struct

  let name = "clock"

  module Foo = CRDT.Clock.Make(Foo)
  module Bar = CRDT.Clock.Make(Bar)

  let run () =
    let foo = Foo.empty in
    let bar = Bar.empty in
    let foo1 = Foo.incr (Foo.incr foo) in
    let bar1 = Bar.incr bar in
    let foo2 = Foo.merge foo1 (Obj.magic bar1) in
    let bar2 = Bar.incr (Bar.incr bar1) in
    let bar3 = Bar.merge bar2 (Obj.magic foo2) in
    let foo3 = Foo.merge foo2 (Obj.magic bar2) in

    test name Foo.contents Foo.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 2);
      ("foo3", foo3, 2);
    ];
    test name Bar.contents Bar.to_string int [
      ("bar1", bar1, 1);
      ("bar2", bar2, 3);
      ("bar3", bar3, 3);
    ]

end

module AddCounterTest = struct

  let name = "add-counter"

  module Foo = CRDT.Counter.Add(Foo)
  module Bar = CRDT.Counter.Add(Bar)

  let run () =
    let foo = Foo.empty in
    let bar = Bar.empty in
    let foo1 = Foo.incr (Foo.incr foo) in
    let bar1 = Bar.incr bar in
    let foo2 = Foo.merge foo1 (Obj.magic bar1) in
    let bar2 = Bar.incr (Bar.incr bar1) in
    let foo3 = Foo.merge foo2 (Obj.magic bar2) in
    let bar3 = Bar.merge bar2 (Obj.magic foo2) in
    test name Foo.contents Foo.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 3);
      ("foo3", foo3, 5);
    ];
    test name Bar.contents Bar.to_string int [
      ("bar1", bar1, 1);
      ("bar2", bar2, 3);
      ("bar3", bar3, 5);
    ]

end

module CounterTest = struct

  let name = "counter"

  module Foo = CRDT.Counter.Make(Foo)
  module Bar = CRDT.Counter.Make(Bar)

  let run () =
    let foo = Foo.empty in
    let bar = Bar.empty in
    let foo1 = Foo.incr (Foo.incr foo) in
    let bar1 = Bar.incr (Bar.incr (Bar.incr bar)) in
    let foo2 = Foo.merge foo1 (Obj.magic bar1) in
    let bar2 = Bar.decr (Bar.decr bar1) in
    let foo3 = Foo.merge foo2 (Obj.magic bar2) in
    let bar3 = Bar.merge bar2 (Obj.magic foo2) in
    test name Foo.contents Foo.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 5);
      ("foo3", foo3, 3);
    ];
    test name Bar.contents Bar.to_string int [
      ("bar1", bar1, 3);
      ("bar2", bar2, 1);
      ("bar3", bar3, 3);
    ]

end

module SetTest = struct

  let name = "set"

  module MyInt = struct
    type t = int
    let compare = (-)
    let to_string = string_of_int
  end
  module Foo = CRDT.Set.Make(Foo)(MyInt)
  module Bar = CRDT.Set.Make(Bar)(MyInt)

  let run () =
    let foo = Foo.empty in
    let bar = Bar.empty in
    let foo1 = Foo.add 2 (Foo.add 1 foo) in
    let bar1 = Bar.add 2 (Bar.add 3 bar) in
    let foo2 = Foo.merge foo1 (Obj.magic bar1) in
    let bar2 = Bar.remove 2 bar1 in
    let foo3 = Foo.remove 3 foo2 in
    let bar3 = Bar.merge bar2 (Obj.magic foo2) in
    let foo4 = Foo.merge foo3 (Obj.magic bar3) in
    let bar4 = Bar.merge bar3 (Obj.magic foo3) in
    test name Foo.elements Foo.to_string (list int) [
      ("foo1", foo1, [1;2]);
      ("foo2", foo2, [1;2;3]);
      ("foo3", foo3, [1;2]);
      ("foo4", foo4, [1;2]);
    ];
    test name Bar.elements Bar.to_string (list int) [
      ("bar1", bar1, [2;3]);
      ("bar2", bar2, [3]);
      ("bar3", bar3, [1;2;3]);
      ("bar4", bar4, [1;2]);
    ]

end

let _ =
  ClockTest     .run ();
  AddCounterTest.run ();
  CounterTest   .run ();
  SetTest       .run ()
;;
