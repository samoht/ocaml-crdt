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
  type key
  type value
  module Value: MERGEABLE with type t := value
  module Map: Map.S with type key := key
  type map = Value.contents Map.t
  include MERGEABLE with type contents := map
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val bindings: t -> (key * Value.contents) list
end

module Make (A: ACTOR) (Key: COMPARABLE) (Value: MERGEABLE with type actor = A.t) =
struct

  type actor = A.t

  module Value = Value

  module S = CRDT_set.Make(A)(Key)
  module Map = Map.Make(Key)

  type key = Key.t

  type value = Value.t

  type map = Value.contents Map.t

  type t = {
    me : actor;
    set: S.t;
    map: value Map.t;
  }

  let contents t =
    Map.fold (fun key value map ->
        if S.mem key t.set && not (Value.is_empty value) then
          Map.add key (Value.contents value) map
        else
          map
      ) t.map Map.empty

  let create me = {
    me;
    map = Map.empty;
    set = S.create me;
  }

  let is_empty t =
    S.is_empty t.set

  let mem key t =
    S.mem key t.set

  (* XXX *)
  let add key value t =
    let set = S.add key t.set in
    let map =
      try
        let v = Map.find key t.map in
        Map.add key (Value.merge value v) (Map.remove key t.map)
      with Not_found ->
        Map.add key value t.map in
    { t with set; map }

  let remove key t =
    if S.mem key t.set then
      let set = S.remove key t.set in
      let map = Map.remove key t.map in
      { t with set; map }
    else
      t

  let to_string t =
    let b = Buffer.create 1024 in
    Printf.bprintf b "{%s| " (A.to_string t.me);
    Map.iter (fun key value ->
        let set = if S.mem key t.set then "" else "*" in
        Printf.bprintf b "%s%s:%s " (Key.to_string key) set (Value.to_string value);
      ) t.map;
    Printf.bprintf b "}";
    Buffer.contents b

  (* XXX *)
  let merge t1 t2 =
    let set = S.merge t1.set t2.set in
    let map = Map.fold (fun key v1 m2 ->
        if not (S.mem key set) then m2
        else
          try
            let v2 = Map.find key t2.map in
            let v = Value.merge v1 v2 in
            Map.add key v (Map.remove key m2)
          with Not_found ->
            Map.add key v1 m2
      ) t1.map t2.map in
    { t1 with set; map }

  let bindings t =
    Map.bindings (contents t)

  let chown t me = {
    me;
    set = S.chown t.set me;
    map = Map.map (fun v -> Value.chown v me) t.map;
  }

end
