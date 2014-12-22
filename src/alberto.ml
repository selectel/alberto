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

(* +-------+
   | Utils |
   +-------+ *)

module BE = EndianString.BigEndian

let (<| ) f x = f x

let failwithf fmt = Printf.kprintf failwith fmt


(** Shortcuts for some of the Binary functions. *)
let pack_byte n =
  let buf = Bytes.create 1 in begin
    BE.set_int8 buf 0 n;
    buf
  end
and pack_word n =
  let buf = Bytes.create 2 in begin
    BE.set_int16 buf 0 n;
    buf
  end
and pack_int32 n =
  let buf = Bytes.create 4 in begin
    BE.set_int32 buf 0 (Int32.of_int n);
    buf
  end


(** [big_int_digits n] returns a list of digits of a given number [n]. *)
let big_int_digits n =
  let offset = big_int_of_int 256 in
  let rec aux n l =
    if eq_big_int n zero_big_int then
      l
    else
      let q, m = quomod_big_int n offset in
      aux q (int_of_big_int m :: l)
  in aux n []


module Stream = struct
  include Stream

  (** [take n st] returns a *sub-stream* of stream [st], which has [n]
      or fewer characters. *)
  let rec take n st =
    if n > 0 then
      slazy (fun _ -> take (n - 1) st) |> icons (next st)
    else
      sempty

  (** [to_string ?length st] returns string representation of a given
      stream [st], using a buffer of a given [length] for conversion. *)
  let to_string ?(length=64) st =
    let buf = Buffer.create length in begin
      iter (fun c -> Buffer.add_char buf c) st;
      Buffer.contents buf
    end

  (** [take_string len st] extracts and returns a string of a given [length]
      from stream [st]. *)
  let take_string length st = take length st |> to_string ~length

  (** [take_byte st] extracts and returns a 1 byte unsigned integer
      from stream [st]. *)
  let take_byte st = BE.get_uint8 (take_string 1 st) 0

  (** [take_word st] extracts and returns a 2 byte unsigned integer
      from stream [st]. *)
  let take_word st = BE.get_uint16 (take_string 2 st) 0

  (** [take_int32 st] extracts and returns a 4 byte signed integer from
      stream [st]. *)
  let take_int32 st = BE.get_int32 (take_string 4 st) 0 |> Int32.to_int

  (** [take_big len st] extracts and returns a [!Bignum] of length
      [len] from stream [st]. *)
  let take_big len st =
    let offset = 256 in
    let rec aux idx n = match idx with
      | 0 -> n
      | _ ->
        if idx < 0 then
          failwithf "Invalid length: %i" idx
        else begin
          let digit = take_byte st in
          aux (idx - 1)
            (add_int_big_int digit (mult_int_big_int offset n))
        end
    in aux len (big_int_of_int 0)

  (** [take_list len f st] extracts and returns a [!List] of items
      fetched by [f] from stream [st]. *)
  let take_list len f st =
    let rec aux idx l = match idx with
      | 0 -> List.rev l
      | _ ->
        if idx < 0 then
          failwithf "Invalid length: %i" idx
        else (f st) :: l |> aux (idx - 1)
    in aux len []
end


module String = struct
  include String

  let resize s n =
    let s' = make n '\000' in

    if length s < n then
      blit s 0 s' 0 (length s)
    else
      blit s 0 s' 0 n;

    s'
end


(* +--------+
   | Parser |
   +--------+ *)

module S = Stream

let rec parse = parser
  (* SMALL_INTEGER_EXT: Unsigned 8 but integer. *)
  | [< ''\097'; n = S.take_byte >] -> `Int n

  (* INTEGER_EXT: Signed 32 bit integer in big-endian format, i.e.
     MSB first. *)
  | [< ''\098'; n = S.take_int32 >] -> `Int n

  (* FLOAT_EXT: A float is stored in string format. the format used
     in sprintf to format the float is "%.20e". *)
  | [< ''\099'; s = S.take_string 31 >] ->
    let zeros = String.index s '\000' in
    `Float (float_of_string <| String.sub s 0 zeros)

  (* ATOM_EXT *)
  | [< ''\100'; len = S.take_word; st >] ->
    if len < 256 then
      `Atom (S.take_string len st)
    else
      failwithf "ATOM_EXT length exceeded %i > 256" len

  (* REFERENCE_EXT *)
  | [< ''\101'; node = parse; id = S.take_string 4; creation = S.take_byte >] ->
    let node = match node with
      | `Atom s -> s
      | #t -> failwithf "Unexpected REFERENCE_EXT format"
    in `Reference (node, id, creation)

  (* PORT_EXT *)
  | [< ''\102'; node = parse; id = S.take_int32; creation = S.take_byte >] ->
    let node = match node with
      | `Atom s -> s
      | #t -> failwithf "Unexpected PORT_EXT format"
    in `Port (node, id, creation)

  (* PID_EXT *)
  | [< ''\103'; node = parse; id = S.take_string 4; serial = S.take_int32;
       creation = S.take_byte >] ->
    let node = match node with
      | `Atom s -> s
      | #t -> failwithf "Unexpected PID_EXT format"
    in `PID (node, id, serial, creation)

  (* SMALL_TUPLE_EXT *)
  | [< ''\104'; arity = S.take_byte; elements = S.take_list arity parse >] ->
    `Tuple elements
  (* LARGE_TUPLE_EXT *)
  | [< ''\105'; arity = S.take_int32 ; elements = S.take_list arity parse >] ->
    `Tuple elements

  (* NIL_EXT *)
  | [< ''\106' >] -> `List []
  (* LIST_EXT *)
  | [< ''\108'; len = S.take_int32; elements = S.take_list len parse;
       ''\106'  (* NIL. *) >] -> `List elements

  (* STRING_EXT *)
  | [< ''\107'; len = S.take_word; s = S.take_string len >] ->
    `String s

  (* BINARY_EXT *)
  | [< ''\109'; len = S.take_int32; s = S.take_string len >] ->
    let buf = Buffer.create len in begin
      Buffer.add_string buf s; `Binary buf
    end

  (* SMALL_BIG_EXT *)
  | [< ''\110'; len = S.take_byte; sign = S.take_byte; n = S.take_big len >] ->
    `Bignum (if sign > 0 then minus_big_int n else n)
  (* LARGE_BIG_EXT *)
  | [< ''\111'; len = S.take_int32; sign = S.take_byte; n = S.take_big len >] ->
    `Bignum (if sign > 0 then minus_big_int n else n)

  (* NEW_REFERENCE_EXT *)
  | [< ''\114'; len = S.take_word; node = parse; creation = S.take_byte;
       id = S.take_string (len * 4) >] ->
    let node = match node with
      | `Atom s -> s
      | #t -> failwithf "Unexpected NEW_REFERENCE_EXT format"
    in `NewReference (node, creation, id)

  (* FUN_EXT *)
  (* NEW_FUN_EXT *)
  (* EXPORT_EXT *)

  (* BIT_BINARY_EXT *)
  | [< ''\077'; len = S.take_int32; bits = S.take_byte; s = S.take_string len >] ->
    let buf = Buffer.create len in begin
      Buffer.add_string buf s;
      `BitBinary (buf, bits)
    end

  (* Unknown tag? *)
  | [< tag = S.take_string 1 >] -> invalid_arg tag


(* +------------+
   | Serializer |
   +------------+ *)

let rec serialize buf term =
  let add f x = Buffer.add_string buf <| f x in
  let open Buffer in match term with
    | `Int n when n >= 0 && n < 256 ->
      begin add pack_byte 97; add pack_byte n end
    | `Int n ->
      begin add pack_byte 98; add pack_int32 n end
    | `Float f ->
      let s = Printf.sprintf "%.20e" f in begin
        add pack_byte 99;
        add_string buf s;
        add_string buf <| String.make (31 - String.length s) '\000'
      end
    | `Atom s ->
      let length = String.length s in
      if length < 256 then (
        add pack_byte 100;
        add pack_word length;
        add_string buf s
      ) else
        failwithf "ATOM_EXT length exceeded %i > 255" length
    | `Reference (node, id, creation) ->
      begin
        add pack_byte 101;
        serialize buf (`Atom node); (* reuse the buf we already have. *)
        Buffer.add_string buf (String.resize id 4);
        add pack_byte creation
      end
    | `Port (node, id, creation) ->
      begin
        add pack_byte 102;
        serialize buf (`Atom node);
        add pack_int32 id;
        add pack_byte creation
      end
    | `PID (node, id, serial, creation) ->
        add pack_byte 103;
        serialize buf (`Atom node);
        Buffer.add_string buf (String.resize id 4);
        add pack_int32 serial;
        add pack_byte creation
    | `Tuple l ->
      let length = List.length l in
      if length < 256 then (
        add pack_byte 104;
        add pack_byte length;
      ) else (
        add pack_byte 105;
        add pack_int32 length;
      );
      List.iter (serialize buf) l
    | `List [] -> add pack_byte 106
    | `List l ->
      begin
        add pack_byte 108;
        add pack_int32 (List.length l);
        List.iter (serialize buf) l;
        add pack_byte 106
      end
    | `String s ->
      let length = String.length s in
      if length < 65536 then (
        add pack_byte 107;
        add pack_word length;
        add_string buf s
      ) else (
          (* Strings longer than 65535 bytes are encoded as LIST_EXT.
             No explicit convertion to [List] is done, because it really
             slows things down for large strings. *)
        add pack_byte 108;
        add pack_int32 (String.length s);
        String.iter (fun c -> add_char buf '\097'; add_char buf c) s;
        add pack_byte 106
      )
    | `Binary b ->
      begin
        add pack_byte 109;
        add pack_int32 (Buffer.length b);
        add_buffer buf b
      end
    | `Bignum n ->
      let sign = match sign_big_int n with -1 -> 1 | _ -> 0 in
      let digits = big_int_digits (abs_big_int n) in
      let length = List.length digits in
      if length < 256 then (
        add pack_byte 110;
        add pack_byte length;
      ) else (
        add pack_byte 111;
        add pack_int32 length;
      );
      add pack_byte sign;
      List.iter (fun d -> add pack_byte d) digits
    | `NewReference (node, creation, id) ->
      let len = (String.length id) / 4 in
      add pack_byte 114;
      add pack_word len;
      serialize buf (`Atom node);
      add pack_byte creation;
      Buffer.add_string buf (String.resize id (len * 4))
    | `BitBinary (b, 0) -> serialize buf (`Binary b)
    | `BitBinary (b, bits) ->
      begin
        add pack_byte 77;
        add pack_int32 (Buffer.length b);
        add pack_byte bits;
        add_buffer buf b
      end


(* +------------+
   | Public API |
   +------------+ *)

let decode_exn s =
  match s.[0] with
    | '\131' ->
      let stream = S.of_string s in
      let (_ : char) = S.next stream in parse stream
    | n   ->
      failwithf "Erlang binary doesn't start with 131: %i" (int_of_char n)

and encode_exn term =
  let buf = Buffer.create 256 in begin
    Buffer.add_char buf '\131';
    serialize buf term;
    Buffer.contents buf
  end


let decode s =
  try
    Some (decode_exn s)
  with _ -> None

and encode term =
  try
    Some (encode_exn term)
  with _ -> None


(* +------------------+
 * | Pretty printers. |
 * +------------------+ *)

let rec to_string = function
  | `Int x -> string_of_int x
  | `Float f -> string_of_float f
  | `Atom s | `String s -> s
  | `Reference (node, id, creation) | `NewReference (node, creation, id) ->
    Printf.sprintf "#Ref<[%s]:%s.%i>" node id creation
  | `Port (node, id, creation) ->
    Printf.sprintf "#Port<[%s]:%i.%i>" node id creation
  | `PID (node, id, serial, creation) ->
    Printf.sprintf "#PID<[%s]:%s.%i.%i>" node id serial creation
  | `Tuple terms ->
    Printf.sprintf "{%s}" (String.concat "," (List.map to_string terms))
  | `List terms ->
    Printf.sprintf "[%s]" (String.concat "," (List.map to_string terms))
  | `Binary b -> Printf.sprintf "<<%s>>" (Buffer.contents b)
  | `Bignum n -> string_of_big_int n
  | `BitBinary (b, bits) -> Printf.sprintf "<<%s:%i>>" (Buffer.contents b) bits


(* +-----------------------------------------+
   | Here come Erlang ports! -- the easy way |
   +-----------------------------------------+ *)

let rec read_term ic =
  let len =
    try
      input_binary_int ic
    with End_of_file -> exit 0 in
  let buf = Bytes.create len in begin
    really_input ic buf 0 len;
    decode_exn buf
  end

and write_term oc term =
  let buf = encode_exn term in begin
    output_binary_int oc (String.length buf);
    output_string oc buf;
    flush oc
  end

and interact f =
  let rec transform state = function
    | `Atom "stop" -> exit 0
    | `Atom "ping" -> state, `Atom "pong"
    | term ->
      let term = try f term with exn ->
        `Tuple [`Atom "error";
                `Tuple [term; `String (Printexc.to_string exn)]]
      in (state, term)
  and loop state =
    let (state, term) = transform state <| read_term stdin in begin
      write_term stdout term;
      loop state
    end
  in begin
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true;

    try loop 0 with End_of_file -> exit 0;

  end
