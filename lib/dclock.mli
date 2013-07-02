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

module type OrderedType = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end

module type S = sig

  (** The type of vector clock *)
  type t

  (** Increment the clock *)
  val incr: t -> t

  (** Merge a local clock with a remote clock *)
  val merge: local:t -> remote:t -> t

  (** Pretty-print the vector clock *)
  val to_string: t -> string

  (** Abstract type for actors *)
  type actor

  (** Create an empty vector clock, owned by the given actor *)
  val create: actor -> t

  (** Change the owner of a given vector clock *)
  val own: actor -> t -> t

  (** Get the local value associated to a clock *)
  val value: t -> int

  (** Fold over the component of the clock *)
  val fold: (actor -> int -> 'a -> 'a) -> t -> 'a -> 'a

end

(** Vector clock builder *)
module Make(A: OrderedType): S
  with type actor = A.t

(** Vector clocks where actors are identified by a string *)
module String: S with type actor = string
