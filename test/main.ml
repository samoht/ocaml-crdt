let count = ref 0

let test name value string_of_x string_of_y tests =
  incr count;
  Printf.printf
    "=====================================\n\
    \  %d. %s\n\
     -------------------------------------\n"
    !count name;
  List.iter (fun (name,x,y) ->
      Printf.printf "%s: %s %s\n" name (string_of_x x) (string_of_y y);
      assert (value x = y)
    ) tests;
  Printf.printf "\n%!"

let int = string_of_int

let list f l =
  let s = List.map f l in
  Printf.sprintf "{ %s }" (String.concat " " s)

module ClockTest = struct

  module C = Vclock.StringActor

  let run name =
    let foo = C.create "foo" in
    let bar = C.create "bar" in
    let foo1 = C.incr (C.incr foo) in
    let bar1 = C.incr bar in
    let foo2 = C.merge foo1 (C.own "foo" bar1) in
    let bar2 = C.incr (C.incr bar1) in
    let bar3 = C.merge bar2 (C.own "bar" foo2) in
    let foo3 = C.merge foo2 (C.own "foo" bar2) in
    test name C.value C.to_string int [
      ("foo1", foo1, 2);
      ("bar1", bar1, 1);
      ("foo2", foo2, 2);
      ("bar2", bar2, 3);
      ("foo3", foo3, 2);
      ("bar3", bar3, 3);
    ]

end

module AddCounterTest = struct

  module C = Dcounter.AddStringActor

  let run name =
    let foo = C.create "foo" in
    let bar = C.create "bar" in
    let foo1 = C.incr (C.incr foo) in
    let bar1 = C.incr bar in
    let foo2 = C.merge foo1 (C.own "foo" bar1) in
    let bar2 = C.incr (C.incr bar1) in
    let bar3 = C.merge bar2 (C.own "bar" foo2) in
    let foo3 = C.merge foo2 (C.own "foo" bar2) in
    test name C.value C.to_string int [
      ("foo1", foo1, 2);
      ("bar1", bar1, 1);
      ("foo2", foo2, 3);
      ("bar2", bar2, 3);
      ("foo3", foo3, 5);
      ("bar3", bar3, 5);
    ]

end

module CounterTest = struct

  module C = Dcounter.StringActor

  let run name =
    let foo = C.create "foo" in
    let bar = C.create "bar" in
    let foo1 = C.incr (C.incr foo) in
    let bar1 = C.incr (C.incr (C.incr bar)) in
    let foo2 = C.merge foo1 (C.own "foo" bar1) in
    let bar2 = C.decr (C.decr bar1) in
    let foo3 = C.merge foo2 (C.own "foo" bar2) in
    let bar3 = C.merge bar2 (C.own "bar" foo2) in
    test name C.value C.to_string int [
      ("foo1", foo1, 2);
      ("bar1", bar1, 3);
      ("foo2", foo2, 5);
      ("bar2", bar2, 1);
      ("foo3", foo3, 3);
      ("bar3", bar3, 3);
    ]

end

module SetTest = struct

  module S = Dset.StringActor.IntSet

  let run name =
    let foo = S.create "foo" in
    let bar = S.create "bar" in
    let foo1 = S.add 2 (S.add 1 foo) in
    let bar1 = S.add 2 (S.add 3 bar) in
    let foo2 = S.merge foo1 (S.own "foo" bar1) in
    let bar2 = S.remove 2 bar1 in
    let foo3 = S.remove 3 foo2 in
    let bar3 = S.merge bar2 (S.own "bar" foo2) in
    let foo4 = S.merge foo3 (S.own "foo" bar3) in
    let bar4 = S.merge bar3 (S.own "bar" foo3) in
    test name S.elements S.to_string (list int) [
      ("foo1", foo1, [1;2]);
      ("bar1", bar1, [2;3]);
      ("foo2", foo2, [1;2;3]);
      ("bar2", bar2, [3]);
      ("foo3", foo3, [1;2]);
      ("bar3", bar3, [1;2;3]);
      ("foo4", foo4, [1;2]);
      ("bar4", bar4, [1;2]);
    ]

end

let _ =
  ClockTest.run "vector";
  AddCounterTest.run "counter";
  CounterTest.run "int";
  SetTest.run "set"
;;
