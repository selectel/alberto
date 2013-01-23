(** Alberto knows how to encode some BERT *)

open Big_int

type t = [ `Int of int
         | `Float of float
         | `Atom of string
         | `Reference of (string * string * int)
         | `Port of (string * int * int)
         | `PID of (string * string * int * int)
         | `Tuple of t list
         | `List of t list
         | `String of string
         | `Binary of Buffer.t
         | `Bignum of big_int
         | `NewReference of (string * int * string)
         | `BitBinary of (Buffer.t * int)
         ]


val encode : t -> string option
(** [encode t] serializes a given Alberto term to a binary string. *)
val decode : string -> t option
(** [decode t] deserializes Alberto term from a binary string. *)

val decode_exn : string -> t
(** [encode_exn t] deserializes Alberto term from a binary string. Raises
    [Failure] if a given string doesn't contain a valid Alberto term. *)
val encode_exn : t -> string
(** [encode_exn t] serializes a given Alberto term to a binary string.
    Raises [Failure] if a given term can't be encoded. *)

val to_string : t -> string
(** [to_string t] converts a given Alberto term to its string representation. *)


val read_term : in_channel -> t
(** [read_term ic] fetches an Erlang [!term] from the port channel [ic]. *)

val write_term : out_channel -> t -> unit
(** [write_term oc term] serializes a given Erlang [!term] to the port
    channel [oc]. *)

val interact : (t -> t) -> unit
(** [interact f] main port loop, recieves Erlang terms from stdin,
    executes a given key function [f] and sends back the results. *)
