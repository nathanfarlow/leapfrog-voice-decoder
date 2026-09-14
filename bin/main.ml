open! Core
open! Async

let convert =
  Command.async
    ~summary:"Convert a voice stream into a wav file"
    (let%map_open.Command rom =
       flag
         "-rom"
         ~doc:"FILE the bin file containing a LFC stream"
         (required Filename_unix.arg_type)
     and offset =
       flag
         "-offset"
         ~doc:"INT the offset into the file where the voice stream exists"
         (required int)
     and output_file =
       flag
         "-output-file"
         ~doc:"FILE the .wav file to write to"
         (required Filename_unix.arg_type)
     in
     fun () ->
       let%bind stream = Reader.file_contents rom >>| String.subo ~pos:offset in
       let wav_bytes = Lfc.Decoder.decode ~stream in
       Writer.save output_file ~contents:wav_bytes)
;;

let convert_all =
  Command.async
    ~summary:"Convert every voice stream in a rom into wav files"
    (let%map_open.Command rom =
       flag
         "-rom"
         ~doc:"FILE the bin file of a cartridge"
         (required Filename_unix.arg_type)
     and output_dir =
       flag
         "-output-dir"
         ~doc:"DIR where to write one <offset>.wav per stream"
         (required Filename_unix.arg_type)
     in
     fun () ->
       let%bind rom = Reader.file_contents rom in
       let offsets =
         Lfc.Rom.stream_offsets rom |> Option.value_exn ~message:"No stream table found"
       in
       let%bind () = Unix.mkdir ~p:() output_dir in
       Deferred.List.iter ~how:`Sequential offsets ~f:(fun offset ->
         let wav_bytes = Lfc.Decoder.decode ~stream:(String.subo rom ~pos:offset) in
         Writer.save (output_dir ^/ sprintf "%06x.wav" offset) ~contents:wav_bytes))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Convert LFC streams"
       [ "convert", convert; "convert-all", convert_all ])
;;
