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
  type t
  type elt
  val empty: t
  val is_empty: t
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val elements: t -> elt list
  module Set: Set.S with type elt := elt
  include MERGEABLE with type contents := Set.t and type t := t
end

module Make (A: ACTOR) (Elt: COMPARABLE) = struct

  module Set = Set.Make(Elt)

  module Map = Map.Make(Elt)

  module Counter = CRDT_counter.Make(A)

  type t = Counter.t Map.t

  type elt = Elt.t

  let contents t =
    Map.fold (fun elt count set ->
        if Counter.contents count > 0 then
          Set.add elt set
        else
          set
      ) t Set.empty

  let empty = Map.empty

  let is_empty t =
    Map.for_all (fun _ count ->
        Counter.contents count = 0
      ) t

  let mem elt t =
    try Counter.contents (Map.find elt t) > 0
    with Not_found -> false

  let add elt t =
    try
      let c = Map.find elt t in
      if Counter.contents c > 0 then
        t
      else
        Map.add elt (Counter.incr c) (Map.remove elt t)
    with Not_found ->
      Map.add elt (Counter.incr Counter.empty) t

  let remove elt t =
    try
      let c = Map.find elt t in
      let i = Counter.contents c in
      if i > 0 then
        Map.add elt (Counter.sub c i) (Map.remove elt t)
      else
        t
    with Not_found ->
      t

  let to_string t =
    let b = Buffer.create 1024 in
    Printf.bprintf b "{%s| " (A.to_string A.me);
    Map.iter (fun key value ->
        Printf.bprintf b "%s:%s " (Elt.to_string key) (Counter.to_string value);
      ) t;
    Printf.bprintf b "}";
    Buffer.contents b

  let merge t1 t2 =
    Map.fold (fun elt i1 t2 ->
        try
          let i2 = Map.find elt t2 in
          let i = Counter.merge i1 i2 in
          Map.add elt i (Map.remove elt t2)
        with Not_found ->
          Map.add elt i1 t2
      ) t1 t2

  let elements t =
    Set.elements (contents t)

  let singleton elt =
    Map.singleton elt (Counter.incr Counter.empty)

  let union t1 t2 =
    Map.fold (fun elt i1 t2 ->
        if Counter.contents i1 > 0 then
          add elt t2
        else
          t2
      ) t1 t2

  let inter = merge

end
