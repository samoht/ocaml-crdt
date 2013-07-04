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

(** Distributed sets. *)

open CRDT_types

(** Distributed sets can be used by a collection of actors to
    concurrently modify a set. The sets are implemented as maps of
    counters, and keep track of how many add and remove operations each
    (abstract) actor have done. *)
module type S = sig

  (** The type of distributed set. *)
  type t

  (** Elements of the distributed sets. *)
  type elt

  (** Distributed sets are sets. TODO: complete the partial
      implementation. *)
  val is_empty: t
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val elements: t -> elt list

  (** Concrete set of elements. *)
  module Set: Set.S with type elt := elt

  (** Distributed sets are mergeable. *)
  include MERGEABLE
    with type contents := Set.t
     and type t := t

end

(** Functor to build a distributed set, from an abstract definition of
    actors and elements of that set. *)
module Make(A: ACTOR)(Elt: COMPARABLE): S with type elt = Elt.t
                                           and type actor = A.t
