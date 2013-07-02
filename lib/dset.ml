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

module Make (A: Dclock.OrderedType) (Elt: Dclock.OrderedType) = struct

  module S = Set.Make(Elt)

  module M = Map.Make(Elt)

  module I = Dint.Make(A)

  type t = {
    me    : A.t;
    counts: I.t M.t;
  }

  let own me t =
    { me; counts = M.map (I.own me) t.counts }

  let value t =
    M.fold (fun elt count set ->
        if I.value count > 0 then
          S.add elt set
        else
          set
      ) t.counts S.empty

  let create h =
    { me = h; counts = M.empty }

  let add_one t elt =
    try
      let c = M.find elt t.counts in
      M.add elt (I.incr c) (M.remove elt t.counts)
    with Not_found ->
      let i = I.create t.me in
      M.add elt (I.incr i) t.counts

  let add t elt =
    { t with counts = add_one t elt }

  let decr_one t elt =
    try
      let c = M.find elt t.counts in
      assert (I.value c > 0);
      M.add elt (I.decr c) (M.remove elt t.counts)
    with Not_found ->
      t.counts

  let remove t elt =
    { t with counts = decr_one t elt }

  let to_string t =
    let b = Buffer.create 1024 in
    let v = value t in
    Printf.bprintf b "{%s| " (A.to_string t.me);
    S.iter (fun elt ->
        Printf.bprintf b "%s " (Elt.to_string elt);
      ) v;
    Printf.bprintf b "}";
    Buffer.contents b

  let merge ~local:t1 ~remote:t2 =
    let counts =
      M.fold (fun elt i1 t2 ->
          try
            let i2 = M.find elt t2 in
            let i = I.merge i1 i2 in
            if I.value i > 0 then M.add elt i (M.remove elt t2)
            else M.remove elt t2
          with Not_found ->
            if I.value i1 > 0 then M.add elt i1 t2
            else t2
        ) t1.counts t2.counts in
    { t1 with counts }

end

module String = struct

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
