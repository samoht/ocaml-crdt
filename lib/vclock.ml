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

exception Bad_owner

module type COMPARABLE = sig
  include Map.OrderedType
  val to_string: t -> string
end

module type MERGEABLE = sig
  type t
  val to_string: t -> string
  val merge: t -> t -> t
  type actor
  val create: actor -> t
  val own: actor -> t -> t
end

module type S = sig
  include MERGEABLE
  val incr: t -> t
  val compare: t -> t -> int option
  val value: t -> int
  val fold: (actor -> int -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (A: COMPARABLE) = struct

  module HMap = Map.Make(A)

  type actor = A.t

  type vector = int HMap.t

  let empty = HMap.empty

  type t = {
    me    : actor;
    vector: vector;
  }

  let create h = {
    me     = h;
    vector = empty;
  }

  let own me t =
    { t with me }

  let incr t =
    let vector =
      try
        let i = HMap.find t.me t.vector in
        HMap.add t.me (i+1) (HMap.remove t.me t.vector)
      with Not_found ->
        HMap.add t.me 1 t.vector in
    { t with vector }

  let value t =
    try HMap.find t.me t.vector
    with Not_found -> 0

  let merge t1 t2 =
    if t1.me <> t2.me then raise Bad_owner;
    let vector =
      HMap.fold (fun h i1 t2 ->
          try
            let i2 = HMap.find h t2 in
            HMap.add h (max i1 i2) (HMap.remove h t2)
          with Not_found ->
            HMap.add h i1 t2
        ) t1.vector t2.vector in
    { t1 with vector }

  let compare t1 t2 =
    let max = merge t1 t2 in
    let is_max t =
      HMap.for_all (fun h i ->
          HMap.mem h t.vector && HMap.find h t.vector = i
        ) max.vector in
    let t1_is_max = is_max t1 in
    let t2_is_max = is_max t2 in
    if t1_is_max && t2_is_max then Some 0
    else if t1_is_max then Some 1
    else if t2_is_max then Some (-1)
    else None

  let to_string t =
    if HMap.is_empty t.vector then "."
    else
      let b = Buffer.create 1024 in
      Printf.bprintf b "[%s| " (A.to_string t.me);
      HMap.iter (fun h i ->
          Printf.bprintf b "%s:%d " (A.to_string h) i;
        ) t.vector;
      Printf.bprintf b "]";
      Buffer.contents b

  let fold f t i =
    HMap.fold f t.vector i

end

module StringActor = Make(struct
    type t = string
    let compare = String.compare
    let to_string x = x
  end)
