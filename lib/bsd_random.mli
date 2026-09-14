(** The random function used in 4.3 bsd with the default seed. *)

open! Core

type t

val create : unit -> t
val next : t -> int
