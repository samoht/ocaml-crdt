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

open CRDT_types

(** The implementation use functional Map over abstract actors so I
    don't expect it to be very efficient. It's more a proof of concept for
    now on. *)

module type S = sig

  (** Vector clocks are mergeable *)
  include MERGEABLE with type contents := int

  (** Increment the clock. *)
  val incr: t -> t

  (** Add a positive integer to the clock. This is equivalent to
      calling [n] times [incr]. If the increment is negative or nul,
      do nothing. *)
  val add: t -> int -> t

  (** Compare two vector clocks. Return [None] if their are not
      comparable. Raise [Bad_owner] if the clocks do not have the same
      owner. *)
  val compare: t -> t -> int option

  (** Fold over the component of the clock. *)
  val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a

end

(** Vector clock builder *)
module Make(A: ACTOR): S
