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

(** Vector clock implementation, based on a abstract representation of
    actors. *)

(** The implementation use functional Map over abstract actors so I
    don't expect it to be very efficient. It's more a proof of concept for
    now on. *)

(** Exception raised when operation with wrong ownership are
    executed. *)
exception Bad_owner

(** Comparable objects are ordered and printable *)
module type COMPARABLE = sig
  include Map.OrderedType
  val to_string: t -> string
end


module type MERGEABLE = sig

  (** Abstract type for mergeable objects. *)
  type t

  (** Pretty-printing. *)
  val to_string: t -> string

  (** Merge two objects together. Both objects should be owned by the
      same actor, otherwise [Bad_owner] is raised. *)
  val merge: t -> t -> t

  (** Abstract type for actors. *)
  type actor

  (** Create an empty object, owned by the given actor. *)
  val create: actor -> t

  (** Change the owner of a given object. *)
  val own: actor -> t -> t

end

module type S = sig

  (** Vector clocks are mergeable *)
  include MERGEABLE

  (** Increment the clock. *)
  val incr: t -> t

  (** Compare two vector clocks. Return [None] if their are not
      comparable. Raise [Bad_owner] if the clocks do not have the same
      owner. *)
  val compare: t -> t -> int option

  (** Get the local value associated to a clock. *)
  val value: t -> int

  (** Fold over the component of the clock. *)
  val fold: (actor -> int -> 'a -> 'a) -> t -> 'a -> 'a

end

(** Vector clock builder *)
module Make(A: COMPARABLE): S
  with type actor = A.t

(** Vector clocks where actors are identified by a string *)
module StringActor: S with type actor = string
