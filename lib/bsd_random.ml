open! Core

type t =
  { state : Int32.t array
  ; mutable f : int
  }

let next ({ state; f } as t) =
  let i = f in
  let j = (i + 28) mod 31 in
  state.(i) <- Int32.(state.(i) + state.(j));
  t.f <- (i + 1) mod 31;
  Int32.(shift_right_logical state.(i) 1 |> to_int_exn)
;;

let create () =
  let state =
    Array.folding_map (Array.create ~len:31 ()) ~init:1l ~f:(fun x () ->
      Int32.((1103515245l * x) + 12345l), x)
  in
  let t = { state; f = 3 } in
  Fn.apply_n_times ~n:310 (fun () -> ignore (next t : int)) ();
  t
;;
