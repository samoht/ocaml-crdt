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

module type ADD = sig
  include Vclock.MERGEABLE
  val value: t -> int
  val incr: t -> t
end

module Add(A: Vclock.COMPARABLE) = struct

  include Vclock.Make(A)

  let value t =
    fold (fun _ -> (+)) t 0

  let to_string t =
    string_of_int (value t)

end

module AddStringActor = Add(struct
    type t = string
    let compare = String.compare
    let to_string x = x
  end)

module type S = sig
  include ADD
  val decr: t -> t
end

module Make(A: Vclock.COMPARABLE) = struct

  type actor = A.t

  module C = Add(A)

  type t = {
    incr: C.t;
    decr: C.t;
  }

  let create h =
    { incr = C.create h; decr = C.create h }

  let own me t = {
    incr = C.own me t.incr;
    decr = C.own me t.decr;
  }

  let incr t =
    { t with incr = C.incr t.incr }

  let decr t =
    { t with decr = C.incr t.decr }

  let value t =
    let incr = C.value t.incr in
    let decr = C.value t.decr in
    incr - decr

  let merge t1 t2 =
    { incr = C.merge t1.incr t2.incr;
      decr = C.merge t1.decr t2.decr; }

  let to_string t =
    string_of_int (value t)

end

module StringActor = Make(struct
    type t = string
    let compare = String.compare
    let to_string x = x
  end)
