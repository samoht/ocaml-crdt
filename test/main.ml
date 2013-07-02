module ClockTest (C: Dclock.S with type actor = string) = struct

  let run name =
    let foo = C.create "foo" in
    let bar = C.create "bar" in
    let foo1 = C.incr (C.incr foo) in
    let bar1 = C.incr bar in
    let foo2 = C.merge foo1 bar1 in
    let bar2 = C.incr (C.incr bar1) in
    let bar3 = C.merge bar2 foo2 in
    let foo3 = C.merge foo2 bar2 in
    Printf.printf
      "=====================================\n\
      \     %s\n\
       foo1: %s\nbar1: %s\nfoo2: %s\nbar2: %s\nfoo3: %s\nbar3: %s\n\n"
      name
      (C.to_string foo1)
      (C.to_string bar1)
      (C.to_string foo2)
      (C.to_string bar2)
      (C.to_string foo3)
      (C.to_string bar3)

end

module VectorTest = ClockTest(Dclock.String)

module CounterTest = ClockTest(Dcounter.String)

module IntTest = struct

  module I = Dint.String

  let run name =
    let foo = I.create "foo" in
    let bar = I.create "bar" in
    let foo1 = I.incr (I.incr foo) in
    let bar1 = I.incr (I.incr (I.incr bar)) in
    let foo2 = I.merge foo1 bar1 in
    let bar2 = I.decr (I.decr bar1) in
    let foo3 = I.merge foo2 bar2 in
    let bar3 = I.merge bar2 foo2 in
    Printf.printf
      "=====================================\n\
      \     %s\n\
       foo1: %s\nbar1: %s\nfoo2: %s\nbar2: %s\nfoo3: %s\nbar3: %s\n\n"
      name
      (I.to_string foo1)
      (I.to_string bar1)
      (I.to_string foo2)
      (I.to_string bar2)
      (I.to_string foo3)
      (I.to_string bar3)

end

module SetTest = struct

  module S = Dset.String.IntSet

  let run name =
    let foo = S.create "foo" in
    let bar = S.create "bar" in
    let foo1 = S.add (S.add foo 1) 2 in
    let bar1 = S.add (S.add bar 3) 2 in
    let foo2 = S.merge foo1 bar1 in
    let bar2 = S.remove bar1 2 in
    let foo3 = S.remove foo2 3 in
    let bar3 = S.merge bar2 foo2 in
    let foo4 = S.merge bar2 foo2 in
    let bar4 = S.merge bar2 foo2 in
    Printf.printf
      "=====================================\n\
      \     %s\n\
       foo1: %s\nbar1: %s\nfoo2: %s\nbar2: %s\nfoo3: %s\nbar3: %s\n\
       foo4: %s\nbar4: %s\n\n"
      name
      (S.to_string foo1)
      (S.to_string bar1)
      (S.to_string foo2)
      (S.to_string bar2)
      (S.to_string foo3)
      (S.to_string bar3)
      (S.to_string foo4)
      (S.to_string bar4)

end

let _ =
  VectorTest.run "vector";
  CounterTest.run "counter";
  IntTest.run "int";
  SetTest.run "set"
;;
