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
  val is_empty: t
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val elements: t -> elt list
  module Set: Set.S with type elt := elt
  include MERGEABLE with type contents := Set.t and type t := t
end

module Make (A: ACTOR) (Elt: COMPARABLE) = struct

  type actor = A.t

  module Set = Set.Make(Elt)

  module Map = Map.Make(Elt)

  module Counter = CRDT_counter.Make(A)

  type t = {
    me : actor;
    set: Set.t;
    map: Counter.t Map.t;
  }

  type elt = Elt.t

  let set_of_map map =
    Map.fold (fun elt count set ->
        if Counter.contents count > 0 then
          Set.add elt set
        else
          set
      ) map Set.empty

  let contents t = t.set

  let create me = {
    me;
    set = Set.empty;
    map = Map.empty;
  }

  let is_empty t =
    Set.is_empty t.set

  let mem elt t =
    Set.mem elt t.set

  let add elt t =
    if Set.mem elt t.set then
      t
    else
      let set = Set.add elt t.set in
      let map =
        try
          let c = Map.find elt t.map in
          Map.add elt (Counter.incr c) (Map.remove elt t.map)
        with Not_found ->
          Map.add elt (Counter.incr (Counter.create t.me)) t.map in
      { t with set; map }

  let remove elt t =
    try
      let set = Set.remove elt t.set in
      let map =
        let c = Map.find elt t.map in
        let i = Counter.contents c in
        if i > 0 then
          Map.add elt (Counter.decrn c i) (Map.remove elt t.map)
        else
          t.map in
      { t with set; map }
    with Not_found ->
      t

  let to_string t =
    let b = Buffer.create 1024 in
    Printf.bprintf b "{%s| " (A.to_string t.me);
    Map.iter (fun key value ->
        Printf.bprintf b "%s:%s " (Elt.to_string key) (Counter.to_string value);
      ) t.map;
    Printf.bprintf b "}";
    Buffer.contents b

  let merge t1 t2 =
    if t1.me <> t2.me then raise Bad_owner
    else
      let map =
        Map.fold (fun elt i1 t2 ->
            try
              let i2 = Map.find elt t2 in
              let i = Counter.merge i1 i2 in
              Map.add elt i (Map.remove elt t2)
            with Not_found ->
              Map.add elt i1 t2
          ) t1.map t2.map in
      let set = set_of_map map in
      { me = t1.me; set; map }

  let elements t =
    Set.elements t.set

  let union t1 t2 =
    if t1.me <> t2.me then raise Bad_owner
    else
      let set = Set.union t1.set t2.set in
      let map = Map.fold Map.add t1.map t2.map in
      { me = t1.me; set; map }

  let inter t1 t2 =
    if t1.me <> t2.me then raise Bad_owner
    else
      let set = Set.inter t1.set t2.set in
      let map = Map.filter (fun k _ -> Map.mem k t2.map) t1.map in
      { me = t1.me; set; map }

  let chown t me =
    { t with me;
             map = Map.map (fun c -> Counter.chown c me) t.map }

end
