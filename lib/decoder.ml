open! Core

let take_u16 = Iobuf.Consume.uint16_le

(** Extract bits from a half word, lsb first. *)
module U16Parser = struct
  type t = { mutable value : int }

  let create value = { value }

  let take t n =
    let value = t.value land ((1 lsl n) - 1) in
    t.value <- t.value lsr n;
    value
  ;;

  let%expect_test _ =
    let t = create 0b1_00_111_0000_111111 in
    let go n = take t n |> printf "%d " in
    List.iter [ 6; 4; 3; 2; 1 ] ~f:go;
    [%expect {| 63 0 7 0 1 |}]
  ;;
end

(** A half word that encodes the type of each of the 6 records in a frame. *)
module RecordConfig = struct
  module Info = struct
    type t =
      { is_present : bool
      ; is_pulsed : bool
      }
    [@@deriving fields ~getters]
  end

  type t = Info.t list

  let parse stream =
    match take_u16 stream with
    | 0x0fc0 -> `End_of_stream
    | mask ->
      let parser = U16Parser.create mask in
      let take_6_bools () =
        Array.init 6 ~f:(fun _ -> U16Parser.take parser 1 <> 0) |> Array.to_list
      in
      let present = take_6_bools () in
      let pulsed = take_6_bools () in
      let records =
        List.zip_exn present pulsed
        |> List.rev_map ~f:(fun (is_present, is_pulsed) -> { Info.is_present; is_pulsed })
      in
      `Ok records
  ;;

  let has_present = List.exists ~f:Info.is_present
end

module Sign = struct
  type t =
    | Positive
    | Negative
  [@@deriving sexp_of]

  let of_int = function
    | 0 -> Positive
    | 1 -> Negative
    | _ -> assert false
  ;;

  let apply t n =
    match t with
    | Positive -> n
    | Negative -> -n
  ;;
end

module StreamConfig = struct
  type t =
    { pulse_mode : int
      (** 0, 1, or 2. Denotes how many pulses per pulsed record: 4, 6, or 8. *)
    ; odd_samples_only : bool
      (** If true, skip every other sample. Why does this exist? *)
    }
  [@@deriving sexp_of]

  let parse stream =
    let word = take_u16 stream in
    let pulse_mode = min (word land 3) 2 in
    let odd_samples_only = word land 4 <> 0 in
    { pulse_mode; odd_samples_only }
  ;;
end

(** A record is 32 samples. *)
module Record = struct
  module Pulses = struct
    type pair =
      { position : int
      ; sign : Sign.t
      }
    [@@deriving sexp_of]

    type t =
      { gain : int
      ; shape : int
      ; pairs : pair list
      }
    [@@deriving sexp_of]
  end

  type t =
    | Pulses of Pulses.t
    | Noise of
        { row : int
        ; gain : int
        ; sign : Sign.t
        }
    | Silent
  [@@deriving sexp_of]

  let parse ~(config : StreamConfig.t) ~(record : RecordConfig.Info.t) stream =
    let make_take () = take_u16 stream |> U16Parser.create |> U16Parser.take in
    match record.is_present, record.is_pulsed with
    | false, false -> Silent
    | true, false ->
      let take = make_take () in
      let row = take 8 in
      let gain = take 5 in
      let sign = take 1 |> Sign.of_int in
      Noise { row; gain; sign }
    | _, true ->
      let take = make_take () in
      let gain = take 5 in
      let shape = take 6 in
      let signs take n = Array.init n ~f:(fun _ -> take 1 |> Sign.of_int) in
      let pairs =
        match config.pulse_mode with
        | 0 ->
          let pos1 = take 5 in
          let take = make_take () in
          let signs = signs take 4 in
          let pos2 = take 4 lsl 1 in
          let pos3 = take 4 lsl 1 in
          let pos4 = take 4 lsl 1 in
          List.zip_exn (Array.to_list signs) [ pos1; pos2; pos3; pos4 ]
          |> List.map ~f:(fun (sign, position) -> { Pulses.sign; position })
        | 1 ->
          let pos1 = take 5 in
          let take = make_take () in
          let signs = signs take 6 in
          let pos2 = take 5 in
          let pos3 = take 5 in
          let take = make_take () in
          let pos4 = take 5 in
          let pos5 = take 5 in
          let pos6 = take 5 in
          List.zip_exn (Array.to_list signs) [ pos1; pos2; pos3; pos4; pos5; pos6 ]
          |> List.map ~f:(fun (sign, position) -> { Pulses.sign; position })
        | 2 ->
          let pos1 = take 5 in
          let take = make_take () in
          let signs = signs take 6 in
          let pos2 = take 5 in
          let pos3 = take 5 in
          let take = make_take () in
          let pos4 = take 5 in
          let pos5 = take 5 in
          let pos6 = take 5 in
          let sign7 = take 1 |> Sign.of_int in
          let take = make_take () in
          let pos7 = take 5 in
          let pos8 = take 5 in
          ignore @@ take 5;
          let sign8 = take 1 |> Sign.of_int in
          List.zip_exn
            (Array.to_list signs @ [ sign7; sign8 ])
            [ pos1; pos2; pos3; pos4; pos5; pos6; pos7; pos8 ]
          |> List.map ~f:(fun (sign, position) -> { Pulses.sign; position })
        | _ -> assert false
      in
      Pulses { gain; shape; pairs }
  ;;
end

module Filter = struct
  type t = int list [@@deriving sexp_of]

  let parse stream =
    let parse_half seps =
      let parser = U16Parser.create (take_u16 stream) in
      List.map seps ~f:(fun sep -> U16Parser.take parser sep)
    in
    let first = parse_half [ 5; 4; 4; 3 ] in
    let second = parse_half [ 3; 3; 3; 3; 2; 2 ] in
    first @ second
  ;;
end

module Frame = struct
  type t =
    { filter : Filter.t option
    ; records : Record.t list (** 6 of them. *)
    }
  [@@deriving sexp_of]

  let parse_header stream =
    match RecordConfig.parse stream with
    | `End_of_stream -> `End_of_stream
    | `Ok record_config ->
      let filter =
        if RecordConfig.has_present record_config
        then Some (Filter.parse stream)
        else None
      in
      `Ok (~record_config, ~filter)
  ;;

  let parse ~config ~(record_config : RecordConfig.t) ~filter stream =
    let next = parse_header stream in
    let records =
      List.map record_config ~f:(fun record -> Record.parse ~config ~record stream)
    in
    ~frame:{ filter; records }, ~next
  ;;
end

module Stream = struct
  type t =
    { config : StreamConfig.t
    ; frames : Frame.t list
    }
  [@@deriving sexp_of]

  let parse stream =
    let config = StreamConfig.parse stream in
    let rec parse_frames = function
      | `End_of_stream -> []
      | `Ok (~record_config, ~filter) ->
        let ~frame, ~next = Frame.parse ~config ~record_config ~filter stream in
        frame :: parse_frames next
    in
    let frames =
      match Frame.parse_header stream with
      | `Ok (~record_config, ~filter:None) as header ->
        (* A strange special case when there are no present records on the first
           frame. *)
        let records =
          List.map record_config ~f:(fun record -> Record.parse ~config ~record stream)
        in
        { Frame.filter = None; records } :: parse_frames header
      | header -> parse_frames header
    in
    { config; frames }
  ;;
end

module Render = struct
  let excitation (config : StreamConfig.t) (record : Record.t) =
    match record with
    | Silent -> List.init 32 ~f:(Fn.const 0)
    | Noise { row; gain; sign } ->
      let loudness = Tables.gradient.(gain) |> Sign.apply sign in
      List.init 32 ~f:(fun i -> Q15.mul loudness Tables.noise.(row).(i))
    | Pulses { gain; shape; pairs } ->
      let excitation = Array.init 32 ~f:(Fn.const 0) in
      let loudest = Tables.gain.(config.pulse_mode).(gain) in
      List.iteri pairs ~f:(fun i { Record.Pulses.position; sign } ->
        let amplitude =
          match i with
          | 0 -> loudest
          | _ -> Q15.mul loudest Tables.shape.(config.pulse_mode).(shape).(i - 1)
        in
        excitation.(position) <- Sign.apply sign amplitude);
      Array.to_list excitation
  ;;

  let filter ~k ~b samples =
    List.map samples ~f:(fun sample ->
      let f =
        List.fold (List.range 9 (-1) ~stride:(-1)) ~init:sample ~f:(fun f j ->
          let f = Q15.(clamp (f + mul k.(j) b.(j))) in
          b.(j + 1) <- Q15.(clamp (b.(j) + mul (-k.(j)) f));
          f)
      in
      b.(0) <- f;
      f)
  ;;

  let go (stream : Stream.t) =
    let k = Array.create ~len:10 0 in
    let b = Array.create ~len:11 0 in
    let records =
      List.concat_map stream.frames ~f:(fun frame ->
        (match frame.filter with
         | None -> ()
         | Some filter ->
           List.iteri filter ~f:(fun i code -> k.(i) <- Tables.k.(i).(code)));
        List.concat_map frame.records ~f:(fun record ->
          filter ~k ~b (excitation stream.config record)))
    in
    let initial_silence = List.init 32 ~f:(Fn.const 0) in
    let decay =
      match List.is_empty records with
      | true -> []
      | false -> filter (List.init 192 ~f:(Fn.const 0)) ~k ~b
    in
    let samples = initial_silence @ records @ decay in
    match stream.config.odd_samples_only with
    | false -> samples
    | true -> List.filteri samples ~f:(fun i _ -> i % 2 = 1)
  ;;
end

let decode ~stream = Iobuf.of_string stream |> Stream.parse |> Render.go |> Wav.encode
