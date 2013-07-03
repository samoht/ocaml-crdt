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
  val add: t -> int -> t
  val incr: t -> t
end

module Add(A: ACTOR): ADD = struct

  module Clock = CRDT_clock.Make(A)

  type t = Clock.t

  type contents = int

  let fold = Clock.fold

  let empty = Clock.empty

  let is_empty = Clock.is_empty

  let incr = Clock.incr

  let add = Clock.add

  let merge = Clock.merge

  let contents t =
    fold (+) t 0

  let to_string t =
    string_of_int (contents t)

end

module type S = sig
  include ADD
  val decr: t -> t
  val add: t -> int -> t
  val sub: t -> int -> t
end

module Make(A: ACTOR): S = struct

  module C = Add(A)

  type t = {
    incr: C.t;
    decr: C.t;
  }

  let empty = {
    incr = C.empty;
    decr = C.empty;
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

  let merge t1 t2 =
    { incr = C.merge t1.incr t2.incr;
      decr = C.merge t1.decr t2.decr; }

  let to_string t =
    Printf.sprintf "[%d|%s|%s]"
      (contents t)
      (C.to_string t.incr)
      (C.to_string t.decr)

  let add t i =
    if i = 0 then t
    else if i > 0 then
      { t with incr = C.add t.incr i }
    else
      { t with decr = C.add t.decr i }

  let sub t i =
    if i = 0 then t
    else if i < 0 then
      { t with incr = C.add t.incr i }
    else
      { t with decr = C.add t.decr i }


end
