open! Core

let samples =
  [ [%blob "samples/00.bin"], [%blob "samples/00.wav"]
  ; [%blob "samples/01.bin"], [%blob "samples/01.wav"]
  ; [%blob "samples/02.bin"], [%blob "samples/02.wav"]
  ; [%blob "samples/03.bin"], [%blob "samples/03.wav"]
  ; [%blob "samples/04.bin"], [%blob "samples/04.wav"]
  ; [%blob "samples/05.bin"], [%blob "samples/05.wav"]
  ; [%blob "samples/06.bin"], [%blob "samples/06.wav"]
  ; [%blob "samples/07.bin"], [%blob "samples/07.wav"]
  ; [%blob "samples/08.bin"], [%blob "samples/08.wav"]
  ; [%blob "samples/09.bin"], [%blob "samples/09.wav"]
  ; [%blob "samples/10.bin"], [%blob "samples/10.wav"]
  ; [%blob "samples/11.bin"], [%blob "samples/11.wav"]
  ]
;;

let%expect_test "All decodings are bit equal to console" =
  List.iteri samples ~f:(fun sample (stream, wav) ->
    if not (String.equal (Lfc.Decoder.decode ~stream) wav)
    then print_s [%message "Differs" (sample : int)]);
  [%expect {| |}]
;;
