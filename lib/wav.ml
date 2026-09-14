open! Core

let encode samples =
  let audio_bytes = List.length samples * 2 in
  let header_bytes = 44 in
  let file_size = header_bytes + audio_bytes in
  let iobuf = Iobuf.create ~len:file_size in
  let open Iobuf.Fill in
  stringo iobuf "RIFF";
  uint32_le_trunc iobuf (file_size - 8);
  stringo iobuf "WAVEfmt ";
  uint32_le_trunc iobuf 16;
  uint16_le_trunc iobuf 1;
  uint16_le_trunc iobuf 1;
  uint32_le_trunc iobuf 8000;
  uint32_le_trunc iobuf 16000;
  uint16_le_trunc iobuf 2;
  uint16_le_trunc iobuf 16;
  stringo iobuf "data";
  uint32_le_trunc iobuf audio_bytes;
  List.iter samples ~f:(fun sample -> int16_le_trunc iobuf sample);
  Iobuf.flip_lo iobuf;
  Iobuf.to_string iobuf
;;
