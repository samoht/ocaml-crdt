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

(** Distributed counters. *)

open CRDT_types

(** Distributed counters can be used by a collection of actor to
    concurrently update a given counter. This module defines two
    functors, abstracted over the type of actors. *)

(** Additive counters support increment operations only: such counters
    are in fact equivalent to vector clocks, that actors can
    incremenent only locally and where the global counter value is
    computed as the sum of all the actor local increments. *)
module type ADD = sig

  (** Additive counters are mergeable *)
  include MERGEABLE with type contents = int

   (** And they support only increment operations. *)
  val incr: t -> t

  (** Add increments to a counter. Do nothing if the increment is
      negative or null. *)
  val incrn: t -> int -> t

  (** Additive counter can be casted from and to vector clocks. *)
  type clock

  (** Convert a counter to a clock. *)
  val to_clock: t -> clock

  (** Convert a clock to a counter. *)
  val of_clock: clock -> t

  (** A counter can be viewed as a clock. *)
  module Clock: CRDT_clock.S with type actor := actor and type t := clock

end

(** Functor to build an additive counter. *)
module Add(A: ACTOR): ADD with type actor = A.t

(** Proper counters support increment and decrement operations. They
    are implemented as two vector clocks, counting each operation
    kinds. The global value of a counter is the difference between the
    global increments and the global decrements. *)
module type S = sig

  (** Counters are mergeable *)
  include MERGEABLE with type contents = int

   (** They support increment operations. *)
  val incr: t -> t

  (** Add increments to a counter. Do nothing if the increment is
      negative or null. *)
  val incrn: t -> int -> t

  (** But they also support decrement operations. *)
  val decr: t -> t

  (** Substraction. *)
  val decrn: t -> int -> t

  (** Normalization: remove all zero counters *)
  val normalize: t -> t

end

(** Build a counter from an abstract actor description. *)
module Make(A: ACTOR): S with type actor = A.t
