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

(** Base signatures. *)

(** Comparable objects are ordered and printable *)
module type COMPARABLE = sig
  include Map.OrderedType
  val to_string: t -> string
end

(** Abstract actors are comparable and have a default elements. *)
module type ACTOR = sig
  include COMPARABLE
end

module type MERGEABLE = sig

  (** The type type of mergeable objects. *)
  type t

  (** The type of contents stored in mergeable objects. *)
  type contents

  (** The type of actors which can modify this object. *)
  type actor

  (** Create a new distributed object, that will be updated by the
      given actor. *)
  val create: actor -> t

  (** Change the owner of a disributed object. *)
  val chown: t -> actor ->  t

  (** Get the contents of an object. *)
  val contents: t -> contents

  (** Pretty-printing. *)
  val to_string: t -> string

  (** Merge two objects together. Both objects should be owned by the
      same actor, otherwise [Bad_owner] is raised. *)
  val merge: t -> t -> t

  (** Test wheter an object is actually empty and can be safely
      forgotten by the system. *)
  val is_empty: t -> bool

end

(** Exception thrown when trying to merge two vector with different
    owners. *)
exception Bad_owner
