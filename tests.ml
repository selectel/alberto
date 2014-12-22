(** Tests for Alberto module. *)

open Kaputt.Abbreviations


module String = struct
  include String

  let resize s n =
    if length s > n
    then sub s 0 n
    else
      let buf = Bytes.make n '\000' in
      blit s 0 buf 0 (length s);
      Bytes.to_string buf
end


module ErlGen = struct
  module G = Gen

  (* Helpers. *)
  let pos_int8  = G.make_int 0 (1 lsl 8  - 1)
  let pos_int16 = G.make_int 0 (1 lsl 16 - 1)

  (* Primitive types. *)
  let small_int = G.map1 (fun x -> `Int x) Alberto.to_string pos_int8
  let integer = G.map1 (fun x -> `Int x) Alberto.to_string
    (G.map1 Int32.to_int string_of_int G.int32)
  let atom = G.map1 (fun s -> `Atom s) Alberto.to_string (G.word pos_int8)
  let float = G.map1 (fun f -> `Float f) Alberto.to_string G.float
  let reference = G.map1 (fun r -> `Reference r) Alberto.to_string
    G.(zip3
       (word pos_int8)
       (word (make_int 0 16))
       pos_int8)
  let port = G.map1 (fun p -> `Port p) Alberto.to_string
    G.(zip3
       (word pos_int8)
       (map1 Int32.to_int string_of_int pos_int32)
       pos_int8)
  let pid = G.map1 (fun p -> `PID p) Alberto.to_string
    G.(zip4
       (word pos_int8)
       (word (make_int 1 4))
       (map1 Int32.to_int string_of_int int32)
       pos_int8)
  let nil = G.map1 (fun _ -> `List []) Alberto.to_string G.unit
  let small_string = G.map1 (fun s -> `String s) Alberto.to_string
    (G.word pos_int16)
  let large_string = G.map1 (fun s -> `String s) Alberto.to_string
    G.(word (make_int (1 lsl 16) (1 lsl 20)))
  let binary = G.map1 (fun b -> `Binary b) Alberto.to_string
    (G.buffer (G.word pos_int16))
  let small_big = G.map1 (fun n -> `Bignum n) Alberto.to_string
    (KaputtNums.Generator.big_int pos_int8)
  let large_big = G.map1 (fun n -> `Bignum n) Alberto.to_string
    (KaputtNums.Generator.big_int pos_int16)
  let new_reference = G.map1
    (fun (n, c) -> `NewReference
        (n, c, Bytes.(to_string @@ create ((length n) * 4))))
    Alberto.to_string
    G.(zip2 (word pos_int8) pos_int8)
  let bit_binary = G.map1 (fun b -> `BitBinary b) Alberto.to_string
    G.(zip2
         (G.buffer (word pos_int8))
         pos_int8)

  (* Compound types. *)
  let term = G.choose_list [small_int; integer; atom; float; port; nil]

  let small_tuple = G.map1 (fun l -> `Tuple l) Alberto.to_string
    (G.list pos_int8 term)
  let large_tuple = G.map1 (fun l -> `Tuple l) Alberto.to_string
    (G.list pos_int16 term)
  let list = G.map1 (fun l -> `List l) Alberto.to_string (G.list pos_int16 term)
end


module Spec = struct
  include Spec

  let id (x, y) = x = y
end


let parse_unparse term = Alberto.decode_exn (Alberto.encode_exn term);;


Test.add_random_test
  ~title:"SMALL_INTEGER_EXT"
  ~nb_runs:500
  ErlGen.small_int
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"INTEGER_EXT"
  ~nb_runs:500
  ErlGen.integer
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"FLOAT_EXT"
  ~nb_runs:500
  ErlGen.float
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"ATOM_EXT"
  ~nb_runs:500
  ErlGen.atom
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"REFERENCE_EXT"
  ErlGen.reference
  parse_unparse
  [Spec.always => function
    | (`Reference (node, id, creation), r) ->
      r = (`Reference (node, (String.resize id 4), creation))
    | (_, _) -> false]
;;


Test.add_random_test
  ~title:"PORT_EXT"
  ErlGen.port
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"PID_EXT"
  ErlGen.pid
  parse_unparse
  [Spec.always => function
    | (`PID (node, id, serial, creation), p) ->
      p = (`PID (node, (String.resize id 4), serial, creation))
    | (_, _) -> false]
;;


Test.add_random_test
  ~title:"SMALL_TUPLE_EXT"
  ErlGen.small_tuple
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"LARGE_TUPLE_EXT"
  ~nb_runs:10
  ErlGen.large_tuple
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"NIL_EXT"
  ErlGen.nil
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"STRING_EXT (small)"
  ~nb_runs:500
  ErlGen.small_string
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"STRING_EXT (large)"
  ~nb_runs:10
  ErlGen.large_string
  parse_unparse
  [Spec.always => function
     | (`String _, `List _) -> true
     | _ -> false]
;;


Test.add_random_test
  ~title:"LIST_EXT"
  ~nb_runs:10
  ErlGen.list
  parse_unparse
  [Spec.always => Spec.id]
;;


Test.add_random_test
  ~title:"BINARY_EXT"
  ErlGen.binary
  parse_unparse
  [Spec.always => function
    | (`Binary b, `Binary b') -> (Buffer.contents b) = (Buffer.contents b')
    | _ -> false]
;;


Test.add_random_test
  ~title:"SMALL_BIG_EXT"
  ErlGen.small_big
  parse_unparse
  [Spec.always => function
    | (`Bignum n, `Bignum n') -> Big_int.eq_big_int n n'
    | _ -> false]
;;


Test.add_random_test
  ~title:"LARGE_BIG_EXT"
  ~nb_runs:10
  ErlGen.large_big
  parse_unparse
  [Spec.always => function
    | (`Bignum n, `Bignum n') -> Big_int.eq_big_int n n'
    | _ -> false]
;;


Test.add_random_test
  ~title:"NEW_REFERENCE_EXT"
  ErlGen.new_reference
  parse_unparse
  [Spec.always => function
    | (`NewReference (node, creation, id), r) ->
      let len = (String.length id) / 4 in
      r = (`NewReference (node, creation, (String.resize id (len * 4))))
    | (_, _) -> false]
;;


Test.add_random_test
  ~title:"BIT_BINARY_EXT"
  ErlGen.bit_binary
  parse_unparse
  [Spec.always => function
    | (`BitBinary (b, 0), `Binary b') ->
      (Buffer.contents b) = (Buffer.contents b')
    | (`BitBinary (b, bits), `BitBinary (b', bits')) ->
      (Buffer.contents b) = (Buffer.contents b') && bits = bits'
    | _ -> false]
;;


let () =
  Test.launch_tests ()
;;
