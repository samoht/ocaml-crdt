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

module type S = sig
  include Vclock.MERGEABLE
  type elt
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val elements: t -> elt list
  module Set: Set.S with type elt := elt
  val value: t -> Set.t
end

module Make (A: Vclock.COMPARABLE) (Elt: Vclock.COMPARABLE) = struct

  module Set = Set.Make(Elt)

  module M = Map.Make(Elt)

  module C = Dcounter.Make(A)

  type actor = A.t

  type elt = Elt.t

  type t = {
    me    : A.t option;
    counts: C.t M.t;
  }

  let own h t =
    { me = Some h;
      counts = M.map (C.own h) t.counts }

  let value t =
    M.fold (fun elt count set ->
        if C.value count > 0 then
          Set.add elt set
        else
          set
      ) t.counts Set.empty

  let empty =
    { me = None; counts = M.empty }

  let is_empty t =
    M.is_empty t.counts

  let create h =
    own h empty

  let mem elt t =
    M.mem elt t.counts

  let add_one t elt =
    try
      let c = M.find elt t.counts in
      M.add elt (C.incr c) (M.remove elt t.counts)
    with Not_found ->
      match t.me with
      | None   -> raise Vclock.Bad_owner
      | Some h ->
        let i = C.create h in
        M.add elt (C.incr i) t.counts

  let add elt t =
    { t with counts = add_one t elt }

  let decr_one t elt =
    try
      let c = M.find elt t.counts in
      assert (C.value c > 0);
      M.add elt (C.decr c) (M.remove elt t.counts)
    with Not_found ->
      t.counts

  let remove elt t =
    { t with counts = decr_one t elt }

  let to_string t =
    let b = Buffer.create 1024 in
    let v = value t in
    let a = match t.me with
      | None   -> ""
      | Some h -> A.to_string h in
    Printf.bprintf b "{%s| " a;
    Set.iter (fun elt ->
        Printf.bprintf b "%s " (Elt.to_string elt);
      ) v;
    Printf.bprintf b "}";
    Buffer.contents b

  let merge t1 t2 =
    let counts =
      M.fold (fun elt i1 t2 ->
          try
            let i2 = M.find elt t2 in
            let i = C.merge i1 i2 in
            if C.value i > 0 then M.add elt i (M.remove elt t2)
            else M.remove elt t2
          with Not_found ->
            if C.value i1 > 0 then M.add elt i1 t2
            else t2
        ) t1.counts t2.counts in
    { t1 with counts }

  let elements t =
    Set.elements (value t)

end

module StringActor = struct

  module Make = Make(struct
      type t = string
      let compare = String.compare
      let to_string x = x
    end)

  module IntSet = Make(struct
      type t = int
      let compare = (-)
      let to_string = string_of_int
    end)

end
