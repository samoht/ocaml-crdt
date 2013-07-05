(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open CRDT_types

module type ADD = sig
  include MERGEABLE with type contents = int
  val incr: t -> t
  val incrn: t -> int -> t
  type clock
  val to_clock: t -> clock
  val of_clock: clock -> t
  module Clock: CRDT_clock.S with type actor := actor and type t := clock
end

module Add(A: ACTOR) = struct

  type actor = A.t

  module Clock = CRDT_clock.Make(A)

  type clock = Clock.t

  type t = {
    clock: Clock.t;
    value: int;
  }

  let to_clock t = t.clock

  type contents = int

  let create a = {
    clock = Clock.create a;
    value = 0;
  }

  let incr t = {
    clock = Clock.incr t.clock;
    value = succ t.value;
  }

  let incrn t i = {
    clock = Clock.incr t.clock;
    value = t.value + i;
  }

  let value_of_clock clock =
    Clock.fold clock (fun _ -> (+)) 0

  let of_clock clock =
    let value = value_of_clock clock in
    { value; clock }

  let merge t1 t2 =
    let clock = Clock.merge t1.clock t2.clock in
    let value = value_of_clock clock in
    { clock; value }

  let contents t =
    t.value

  let to_string t =
    string_of_int t.value

  let is_empty t =
    t.value = 0

  let chown t a =
    let clock = Clock.chown t.clock a in
    { t with clock }

end

module type S = sig
  include MERGEABLE with type contents = int
  val incr: t -> t
  val incrn: t -> int -> t
  val decr: t -> t
  val decrn: t -> int -> t
  val normalize: t -> t
end

module Make(A: ACTOR) = struct

  type actor = A.t

  module C = Add(A)

  type t = {
    incr: C.t;
    decr: C.t;
  }

  let create a = {
    incr = C.create a;
    decr = C.create a;
  }

  type contents = int

  let incr t =
    { t with incr = C.incr t.incr }

  let decr t =
    { t with decr = C.incr t.decr }

  let contents t =
    let incr = C.contents t.incr in
    let decr = C.contents t.decr in
    incr - decr

  let is_empty t =
    contents t = 0

  let normalize t =
    let incr = C.to_clock t.incr in
    let decr = C.to_clock t.decr in
    let incr = C.Clock.filter incr (fun a i ->
        C.Clock.mem decr a && C.Clock.find decr a <> i
      ) in
    let decr = C.Clock.filter decr (fun a i ->
        C.Clock.mem incr a && C.Clock.find incr a <> i
      ) in
    let incr = C.of_clock incr in
    let decr = C.of_clock decr in
    { incr; decr }

  let merge t1 t2 =
    let t = {
      incr = C.merge t1.incr t2.incr;
      decr = C.merge t1.decr t2.decr;
    } in
    normalize t

  let to_string t =
    Printf.sprintf "[%d|%s|%s]"
      (contents t)
      (C.to_string t.incr)
      (C.to_string t.decr)

  let incrn t i =
    if i = 0 then t
    else if i > 0 then
      { t with incr = C.incrn t.incr i }
    else
      { t with decr = C.incrn t.decr i }

  let decrn t i =
    if i = 0 then t
    else if i < 0 then
      { t with incr = C.incrn t.incr i }
    else
      { t with decr = C.incrn t.decr i }


  let chown t a = {
    incr = C.chown t.incr a;
    decr = C.chown t.decr a;
  }

end
