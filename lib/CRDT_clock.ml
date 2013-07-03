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

module type S = sig
  include MERGEABLE with type contents := int
  val incr: t -> t
  val add: t -> int -> t
  val compare: t -> t -> int option
  val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (A: ACTOR) = struct

  module ActorMap = Map.Make(A)

  type t = int ActorMap.t

  let empty = ActorMap.empty

  let is_empty = ActorMap.is_empty

  let add t x =
    if x <= 0 then t
    else
      try
        let i = ActorMap.find A.me t in
        ActorMap.add A.me (i+x) (ActorMap.remove A.me t)
      with Not_found ->
        ActorMap.add A.me x t

  let incr t =
    add t 1

  let contents t =
    try ActorMap.find A.me t
    with Not_found -> 0

  let merge t1 t2 =
    ActorMap.fold (fun h i1 t2 ->
        try
          let i2 = ActorMap.find h t2 in
          ActorMap.add h (max i1 i2) (ActorMap.remove h t2)
        with Not_found ->
          ActorMap.add h i1 t2
      ) t1 t2

  let compare t1 t2 =
    let max = merge t1 t2 in
    let is_max t =
      ActorMap.for_all (fun h i ->
          ActorMap.mem h t && ActorMap.find h t = i
        ) max in
    let t1_is_max = is_max t1 in
    let t2_is_max = is_max t2 in
    if t1_is_max && t2_is_max then Some 0
    else if t1_is_max then Some 1
    else if t2_is_max then Some (-1)
    else None

  let to_string t =
    if ActorMap.is_empty t then "."
    else
      let b = Buffer.create 1024 in
      Printf.bprintf b "[%s| " (A.to_string A.me);
      ActorMap.iter (fun h i ->
          Printf.bprintf b "%s:%d " (A.to_string h) i;
        ) t;
      Printf.bprintf b "]";
      Buffer.contents b

  let fold f t i =
    ActorMap.fold (fun _ -> f) t i

end
