open! Core
open Js_of_ocaml

let () =
  Js.Unsafe.set
    Js.Unsafe.global
    (Js.string "streamOffsets")
    (Js.wrap_callback (fun rom ->
       Lfc.Rom.stream_offsets (Typed_array.String.of_uint8Array rom)
       |> Option.value ~default:[]
       |> Array.of_list
       |> Js.array));
  Js.Unsafe.set
    Js.Unsafe.global
    (Js.string "convert")
    (Js.wrap_callback (fun rom offset ->
       let stream = String.subo (Typed_array.String.of_uint8Array rom) ~pos:offset in
       Js.bytestring (Lfc.Decoder.decode ~stream)))
;;
