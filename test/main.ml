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

module MyString = struct
  type t = string
  let compare = String.compare
  let to_string x = x
end

module ClockTest = struct

  let name = "clock"

  module M = CRDT.Clock.Make(MyString)

  let run () =
    let foo = M.create "foo" in
    let bar = M.create "bar" in
    let foo1 = M.incr (M.incr foo) in
    let bar1 = M.incr bar in
    let foo2 = M.merge foo1 (M.chown bar1 "foo") in
    let bar2 = M.incr (M.incr bar1) in
    let bar3 = M.merge bar2 (M.chown foo2 "bar") in
    let foo3 = M.merge foo2 (M.chown bar2 "foo") in

    test name M.contents M.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 2);
      ("foo3", foo3, 2);
      ("bar1", bar1, 1);
      ("bar2", bar2, 3);
      ("bar3", bar3, 3);
    ]

end

module AddCounterTest = struct

  let name = "add-counter"

  module M = CRDT.Counter.Add(MyString)

  let run () =
    let foo = M.create "foo" in
    let bar = M.create "bar" in
    let foo1 = M.incr (M.incr foo) in
    let bar1 = M.incr bar in
    let foo2 = M.merge foo1 (M.chown bar1 "foo") in
    let bar2 = M.incr (M.incr bar1) in
    let foo3 = M.merge foo2 (M.chown bar2 "foo") in
    let bar3 = M.merge bar2 (M.chown foo2 "bar") in
    test name M.contents M.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 3);
      ("foo3", foo3, 5);
      ("bar1", bar1, 1);
      ("bar2", bar2, 3);
      ("bar3", bar3, 5);
    ]

end

module CounterTest = struct

  let name = "counter"

  module M = CRDT.Counter.Make(MyString)

  let run () =
    let foo = M.create "foo" in
    let bar = M.create "bar" in
    let foo1 = M.incr (M.incr foo) in
    let bar1 = M.incr (M.incr (M.incr bar)) in
    let foo2 = M.merge foo1 (M.chown bar1 "foo") in
    let bar2 = M.decr (M.decr bar1) in
    let foo3 = M.merge foo2 (M.chown bar2 "foo") in
    let bar3 = M.merge bar2 (M.chown foo2 "bar") in
    test name M.contents M.to_string int [
      ("foo1", foo1, 2);
      ("foo2", foo2, 5);
      ("foo3", foo3, 3);
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
  module M = CRDT.Set.Make(MyString)(MyInt)

  let run () =
    let foo = M.create "foo" in
    let bar = M.create "bar" in
    let foo1 = M.add 2 (M.add 1 foo) in
    let bar1 = M.add 2 (M.add 3 bar) in
    let foo2 = M.merge foo1 (M.chown bar1 "foo") in
    let bar2 = M.remove 2 bar1 in
    let foo3 = M.remove 3 foo2 in
    let bar3 = M.merge bar2 (M.chown foo2 "bar") in
    let foo4 = M.merge foo3 (M.chown bar3 "foo") in
    let bar4 = M.merge bar3 (M.chown foo3 "bar") in
    test name M.elements M.to_string (list int) [
      ("foo1", foo1, [1;2]);
      ("foo2", foo2, [1;2;3]);
      ("foo3", foo3, [1;2]);
      ("foo4", foo4, [1;2]);
      ("bar1", bar1, [2;3]);
      ("bar2", bar2, [3]);
      ("bar3", bar3, [1;2;3]);
      ("bar4", bar4, [1;2]);
    ]

end

module MapTest = struct

  let name = "map"

  module M = CRDT.Map.Make(MyString)(MyString)(CounterTest.M)

  let run () =
    ()
end

let _ =
  ClockTest     .run ();
  AddCounterTest.run ();
  CounterTest   .run ();
  SetTest       .run ();
  MapTest       .run ()

;;
