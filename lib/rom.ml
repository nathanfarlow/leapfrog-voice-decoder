open! Core

(* Fuzzy find the big table of voice streams.

   The streams have the structure:
   id, 0x0100, 0, flags, count followed by [count] voice pointers.

   And we also know all voice streams must start with a half word of 0, 1, or 2. *)

let stream_offsets rom =
  let rom = Iobuf.of_string rom in
  let length = Iobuf.length rom in
  let u32 pos = Iobuf.Peek.uint32_le rom ~pos in
  let table_at off =
    let magic = u32 (off + 4) in
    let count = u32 (off + 16) in
    if magic <> 0x0100 || count >= 0x4000
    then None
    else (
      let cart_base = 0x80000000 in
      let offsets =
        List.init count ~f:(fun i -> u32 (off + 20 + (4 * i)) - cart_base)
        |> List.dedup_and_sort ~compare:Int.compare
      in
      let is_stream pos =
        pos >= 0 && pos < length - 1 && Iobuf.Peek.uint16_le rom ~pos <= 2
      in
      Option.some_if (List.for_all offsets ~f:is_stream) offsets)
  in
  Sequence.range 0 (min length 0x80000 - 20) ~stride:4
  |> Sequence.filter_map ~f:table_at
  |> Sequence.max_elt ~compare:(Comparable.lift Int.compare ~f:List.length)
;;
