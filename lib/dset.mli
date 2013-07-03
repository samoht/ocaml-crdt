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

(** Distributed sets can be used by a collection of actors to
    concurrently modify a set. The sets are implemented as maps of
    counters, and keep track of how many add and remove operations each
    (abstract) actor have done. *)
module type S = sig

  (** Distributed sets are mergeable *)
  include Vclock.MERGEABLE

  (** The type of the set elements. *)
  type elt

  (** The empty set. *)
  val empty: t

  (** Test whether a set is empty or not. *)
  val is_empty: t -> bool

  (** [mem x s] tests whether [x] belongs to the set [s]. *)
  val mem: elt -> t -> bool

  (** [add x s] returns a set containing all elements of [s], plus
      [x]. If [x] was already in [s], [s] is returned unchanged. *)
  val add: elt -> t -> t

  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned
      unchanged. *)
  val remove : elt -> t -> t

  (** Return the list of all elements of the given set. The returned
      list is sorted in increasing order with respect to the ordering
      [Elt.compare], where [Elt] is the argument given to
      [Dset.Make]. *)
  val elements: t -> elt list

  (** Concrete set of elements. *)
  module Set: Set.S with type elt := elt

  (** Return the concrete set represented by the distributed set. *)
  val value: t -> Set.t

end

(** Functor to build a distributed set, from an abstract definition of
    actors and elements of that set. *)
module Make(A: Vclock.COMPARABLE)(Elt: Vclock.COMPARABLE): S
  with type actor = A.t
   and type elt = Elt.t

(** Distributed sets for string actors. *)
module StringActor: sig

  (** Create a distributed set for a given type of elements *)
  module Make (Elt: Vclock.COMPARABLE): S
    with type actor = string
     and type elt = Elt.t

  (** Distributed integer sets *)
  module IntSet: S
    with type actor = string
     and type elt = int

end
